#' Remove bots and/or duplicates from a data frame
#' 
#' This function removes respondents suspected of being bots or duplicate 
#' survey takers, as well as previews and people under 18 from a data frame. 
#' It is made specifically for surveys programmed with Qualtrics. 
#' 
#' @param data A data.frame object you want to operate on
#' 
#' @export

remove_bot_dupe <- function(data) {

  # remove previews
  if ("DistributionChannel" %in% colnames(data)) {
    data <- data %>% dplyr::filter(DistributionChannel != "preview")
  } else if ("distribution_channel" %in% colnames(data)) {
    data <- data %>% dplyr::filter(distribution_channel != "preview")
  }

  # get the people who are above 18
  if ("age" %in% colnames(data)) {

    if (!is.null(attr_val_labels(df$age))) {

      df <- df %>%
        dplyr::mutate(
          # age, adding 17 so the ages start at 18
          age_n = 17 + age %>% 
            structure(label = "Age")
        ) %>% 
        dplyr::filter(age_n > 17)

    } else if (is.null(attr_val_labels(df$age))) {

      df <- df %>% 

        dplyr::filter(age > 17)
    }

  } 

  

  # remove bots
  if ("Q_RecaptchaScore" %in% colnames(data)) {
    data <- data %>% dplyr::filter(Q_RecaptchaScore > 0.41)
  } else if ("q_recaptcha_score" %in% colnames(data)) {
    data <- data %>% dplyr::filter(q_recaptcha_score > 0.41)
  }

  # remove duplicates
  if ("Q_RelevantIDDuplicate" %in% colnames(data)) {
    data <- data %>% dplyr::filter(Q_RelevantIDDuplicate != "true")
  } else if ("q_relevant_id_duplicate" %in% colnames(data)) {
    data <- data %>% dplyr::filter(q_relevant_id_duplicate != "true")
  }

  return(data)

}



#' Get the bots or duplicates from a data frame
#' 
#' This function is the opposite of `remove_bot_dupes()`. Instead of removing bots
#' and duplicates from a data frame, it keeps them. This was designed to make it easy
#' to send to survey panel providers so they can remove them from your data set.
#' 
#' @param data A data.frame object you want to operate on
#' 
#' @export
get_bot_dupe <- function(data) {

  # remove preview responses
  if ("DistributionChannel" %in% colnames(data)) {
    data <- data %>% dplyr::filter(DistributionChannel != "preview")
  } else if ("distribution_channel" %in% colnames(data)) {
    data <- data %>% dplyr::filter(distribution_channel != "preview")
  }


  # get the clean data
  clean <- remove_bot_dupe(data)
  # remove clean data from original data
  bots <- dplyr::anti_join(data, clean)
  # return the bad data
  return(bots)

}



#' Export data frame with only bots and duplicates
#' 
#' This function creates a file containing all responses suspected of being bots and/or
#' duplicates. Can create a .xlsx, .sav, or .csv file
#' 
#' @param data A data.frame object you want to operate on
#' @param filename The name of the file you want to create
#' @param export_raw_data Logical. If TRUE, the default, exports the raw data. If FALSE,
#'   exports only three columns: the ID, duplicate, and bot
#' @param id The unique ID variable. Only relevant when `export_raw_data` is FALSE.
#' 
#' @export
export_bot_dupe <- function(data, filename, export_raw_data = TRUE, id = NULL) {

  data <- get_bot_dupe(data)
  
  if (isFALSE(export_raw_data)) {
    data <- data %>% 
      dplyr::select(tidyselect::all_of(id), duplicate, bot)
  }

  if (grepl(".xlsx", filename, fixed = TRUE)) {
    writexl::write_xlsx(data, path = filename)
  } else if (grepl(".sav", filename, fixed = TRUE)) {
    haven::write_sav(data, filename)
  } else if (grepl(".csv", filename, fixed = TRUE)) {
    readr::write_csv(data, filename)
  } 
  
  

}
