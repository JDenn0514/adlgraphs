#' Remove bots and/or duplicates from a data frame
#' 
#' `r lifecycle::badge("deprecated")`
#' 
#' `remove_bot_dupe()` is deprecated because the function `remove_bogus()`
#' does the same thing but also removes speedsters. 
#' 
#' This funciton removes respondents suspected of being bots or duplicate 
#' survey takers, as well as previews and people under 18 from a data frame. 
#' It is made specifically for surveys programmed with Qualtrics. 
#' 
#' @param data A data.frame object you want to operate on
#' 
#' @export
remove_bot_dupe <- function(data) {

  lifecycle::deprecate_soft("0.3.61", "remove_bot_dupe()", "remove_bogus")

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
      
      data <- data[data$age_n > 17,]

    } else if (is.null(attr_val_labels(data$age))) {

      data <- data[data$age > 17,]

    }

  } 

  

  # remove bots
  if ("Q_RecaptchaScore" %in% colnames(data)) {
    data <- data[data$Q_RecaptchaScore > 0.41,]
  } else if ("q_recaptcha_score" %in% colnames(data)) {
    data <- data[data$q_recaptcha_score > 0.41,]
  }

  # remove duplicates
  if ("Q_RelevantIDDuplicateScore" %in% colnames(data)) {
    data <- data[data$Q_RelevantIDDuplicateScore < 76,]
  } else if ("q_relevant_id_duplicate" %in% colnames(data)) {
    data <- data[data$q_relevant_id_duplicate_score < 76,]
  }

  return(data)

}




#' Get the bots or duplicates from a data frame
#' 
#' `r lifecycle::badge("deprecated")`
#' 
#' `get_bot_dupe()` is deprecated because the function `get_bogus()`
#' does the same thing but also removes speedsters. 
#' 
#' This function is the opposite of `remove_bot_dupes()`. Instead of removing bots
#' and duplicates from a data frame, it keeps them. This was designed to make it easy
#' to send to survey panel providers so they can remove them from your data set.
#' 
#' @param data A data.frame object you want to operate on
#' 
#' @export
get_bot_dupe <- function(data) {

  lifecycle::deprecate_soft("0.3.61", "get_bot_dupe()", "get_bogus()")

  # remove previews
  if ("DistributionChannel" %in% colnames(data)) {
    data <- data[data$DistributionChannel != "preview",]
  } else if ("distribution_channel" %in% colnames(data)) {
    data <- data[data$distribution_channel != "preview",]
  }


  # get the clean data
  clean <- remove_bot_dupe(data)
  # remove clean data from original data
  bots <- dplyr::anti_join(data, clean)

  # create the two new variables
  bots$duplicate <- ifelse(bots$Q_RelevantIDDuplicateScore < 0.76, TRUE, FALSE)
  bots$bot <- ifelse(bots$Q_RecaptchaScore > 0.41, TRUE, FALSE)

  # return the bad data
  return(bots)

}



#' Export data frame with only bots and duplicates
#' 
#' `r lifecycle::badge("superseded")`
#' 
#' `export_bot_dupe()` is superseded because the function `export_bogus()` 
#' does the same thing but also removes speedsters. 
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

  lifecycle::deprecate_soft("0.3.61", "export_bot_dupe()", "export_bogus")

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
