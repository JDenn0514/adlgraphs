#' Remove bots, duplicates, and/or speedsters from a data frame
#' 
#' This function removes respondents suspected of being bots or duplicate 
#' survey takers, as well as previews, people under 18, and speedsters
#' from a data frame. It is made specifically for surveys programmed with 
#' Qualtrics. 
#' 
#' @param data A data frame.
#' @param duration The name of the time duration variable. Must be a string.
#' @param cut_off Specify what percentage of the median time should be used
#'   to remove speedsters. Default is 0.3, which means people who's time to
#'   complete is 0.3 that of the median completion time are removed.
#' @param only_finished Logical. If `TRUE`, the default, removes all 
#'   respondents who did not finish according to the `finished`/`Finished`
#'   variable. If `FALSE`, keeps those who did not finish.
#' @export
remove_bogus <- function(data, duration, cut_off = 0.3, only_finished = TRUE) {

  data_name <- x_expression(data)
  
  if (only_finished) {
    # if only_finished is TRUE, keep only people who finished
    if ("Finished" %in% colnames(data)) {
      # check the class
      if ("double" %in% class(data$Finished)) {
        data <- data[data$Finished == 1,]
      } else if ("logical" %in% class(data$Finished)) {
        data <- data[data$Finished == TRUE,]
      }
    } else if ("finished" %in% colnames(data)) {
      # check the class
      if ("double" %in% class(data$finished)) {
        data <- data[data$finished == 1,]
      } else if ("logical" %in% class(data$finished)) {
        data <- data[data$finished == TRUE,]
      }
    }
  }

  # remove previews
  if ("DistributionChannel" %in% colnames(data)) {
    data <- data[data$DistributionChannel != "preview",]
  } else if ("distribution_channel" %in% colnames(data)) {
    data <- data[data$distribution_channel != "preview",]
  }

  # get the people who are above 18
  if ("age" %in% colnames(data)) {

    if (!is.null(attr_val_labels(data$age))) {

      data <- data %>%
        dplyr::mutate(
          # age, adding 17 so the ages start at 18
          age_n = 17 + age %>% 
            structure(label = "Age")
        ) 
      
      data <- data[data$age_n > 17 & !is.na(data$age_n),]

    } else if (is.null(attr_val_labels(data$age))) {

      data <- data[data$age > 17 & !is.na(data$age),]
      

    }

  } 

  # remove bots
  if ("Q_RecaptchaScore" %in% colnames(data)) {
    data <- data[data$Q_RecaptchaScore >= 0.5,]
  } else if ("q_recaptcha_score" %in% colnames(data)) {
    data <- data[data$q_recaptcha_score >= 0.5,]
  }

  # remove duplicates
  if ("Q_RelevantIDDuplicateScore" %in% colnames(data)) {
    data <- data[data$Q_RelevantIDDuplicateScore < 75,]
  } else if ("q_relevant_id_duplicate_score" %in% colnames(data)) {
    data <- data[data$q_relevant_id_duplicate_score < 75,]
  }

  # remove fraudulent and bots
  if ("Q_RelevantIDFraudScore" %in% colnames(data)) {
    data <- data[data$Q_RelevantIDFraudScore < 30,]
  } else if ("q_relevant_id_fraud_score" %in% colnames(data)) {
    data <- data[data$q_relevant_id_fraud_score < 30,]
  }



  
  if (!missing(duration)) {
    # if duration is not found in data give an error
    if (!duration %in% colnames(data)) {
      cli::cli_abort(c(
        "`{duration}` is not a variable found in `{data_name}`.",
        "i" = "Make sure the variable supplied to {.var duration} is present in {.var data}"
      ))
    }
    # cut off time
    cut_off_time <- stats::median(data[[duration]]) * cut_off
    # remove speedsters
    data <- data[data[[duration]] > cut_off_time,]
  }
  
  data
}


#' Get bogus data from a data frame
#' 
#' This function is the opposite of `remove_bogus()`. Instead of removing bots,
#' duplicates, and speedster from a data frame, it keeps them. This was designed to 
#' make it easy to send to survey panel providers so they can remove them from 
#' your data set.
#' 
#' @param data A data.frame object you want to operate on
#' @param duration The name of the time duration variable. Must be a string.
#' @param cut_off Specify what percentage of the median time should be used
#'   to remove speedsters. Default is 0.3, which means people who's time to
#'   complete is 0.3 that of the median completion time are removed.
#' 
#' @export
get_bogus <- function(data, duration, cut_off = 0.3) {

  # remove previews
  if ("DistributionChannel" %in% colnames(data)) {
    data <- data[data$DistributionChannel != "preview",]
  } else if ("distribution_channel" %in% colnames(data)) {
    data <- data[data$distribution_channel != "preview",]
  }


  # get the clean data
  clean <- remove_bogus(data, duration = duration, cut_off = cut_off)
  # remove clean data from original data
  bogus <- dplyr::anti_join(data, clean)

  # create the two new variables
  if ("Q_RelevantIDDuplicateScore" %in% colnames(data)) {
    bogus$duplicate <- ifelse(bogus$Q_RelevantIDDuplicateScore > 0.75, TRUE, FALSE)
  } else if ("q_relevant_id_duplicate_score" %in% colnames(data)) {
    bogus$duplicate <- ifelse(bogus$q_relevant_id_duplicate_score > 0.75, TRUE, FALSE)
  }

  if ("Q_RecaptchaScore" %in% colnames(data)) {
    bogus$bot <- ifelse(bogus$Q_RecaptchaScore < 0.5, TRUE, FALSE)
  } else if ("q_recaptcha_score" %in% colnames(data)) {
    bogus$bot <- ifelse(bogus$q_recaptcha_score < 0.5, TRUE, FALSE)
  }

  # get the cut off time
  cut_off_time <- stats::median(data[[duration]]) * cut_off
  # add a new variable if they were speedsters
  bogus$speedsters <- ifelse(bogus[[duration]] > cut_off_time, TRUE, FALSE)
  # return the bad data
  bogus

}



#' Export data frame with only bots and duplicates
#' 
#' This function creates a file containing all responses suspected of being bots,
#' duplicates, or speedsters. Can create a .xlsx, .sav, or .csv file. It uses 
#' `get_bogus()` under the hood.
#' 
#' @param data A data.frame object you want to operate on
#' @param filename The name of the file you want to create
#' @param export_raw_data Logical. If TRUE, the default, exports the raw data. If FALSE,
#'   exports only three columns: the ID, duplicate, and bot
#' @param id The unique ID variable. Only relevant when `export_raw_data` is FALSE.
#' @param duration The name of the time duration variable. Must be a string.
#' @param cut_off Specify what percentage of the median time should be used
#'   to remove speedsters. Default is 0.3, which means people who's time to
#'   complete is 0.3 that of the median completion time are removed.
#' 
#' @export
export_bogus <- function(data, filename, export_raw_data = TRUE, id = NULL, duration, cut_off = 0.3) {

  data <- get_bogus(data, duration, cut_off = cut_off)
  
  if (isFALSE(export_raw_data)) {
    data <- data %>% 
      dplyr::select(tidyselect::all_of(id), duplicate, bot, speedsters)
  }

  if (grepl(".xlsx", filename, fixed = TRUE)) {
    writexl::write_xlsx(data, path = filename)
  } else if (grepl(".sav", filename, fixed = TRUE)) {
    haven::write_sav(data, filename)
  } else if (grepl(".csv", filename, fixed = TRUE)) {
    readr::write_csv(data, filename)
  } 
  
}


write
