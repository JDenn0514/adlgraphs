#' These are a set of functions that make it easy to access attributes/metadata
#' from a vector or column in a `data.frame`. Note: these functions do not allow
#' you to change, set, or remove attributes.
#'
#' @param x A vector object or the name of a column in a data.frame
#' @param df A `data.frame` or `tibble` object. This should be specified when `x`
#'   is only the name of a column.
#'
#' @name attributes
#' @aliases NULL
NULL


#' Get the variable label attribute
#' @rdname attributes
#' @export
attr_var_label <- function(x, df) {
  if (missing(df)) {
    attributes(x)$label
  } else {
    attributes(df[[x]])$label
  }
}

#' Get the value labels attribute
#' @rdname attributes
#' @export
attr_val_labels <- function(x, df) {
  if (missing(df)) {
    attributes(x)$labels
  } else {
    attributes(df[[x]])$labels
  }
}

#' Get the factor levels attribute from a vector
#' @rdname attributes
#' @export
attr_levels <- function(x, df) {
  if (missing(df)) {
    attributes(x)$levels
  } else {
    attributes(df[[x]])$levels
  }
}

#' Get the transformation attribute from a vector
#' @rdname attributes
#' @export
attr_transformation <- function(x, df) {
  if (missing(df)) {
    attributes(x)$transformation
  } else {
    attributes(df[[x]])$transformation
  }
}

#' Get the note attribute
#' @rdname attributes
#' @export
attr_note <- function(x, df) {
  if (missing(df)) {
    attributes(x)$note
  } else {
    attributes(df[[x]])$note
  }
}

#' get question preface attribute
#' @rdname attributes
#' @export
attr_question_preface <- function(x, df) {
  if (missing(df)) {
    attributes(x)$question_preface
  } else {
    attributes(df[[x]])$question_preface
  }
}


# get survey_flow attribute
#' @rdname attributes
#' @export
attr_survey_flow <- function(x, df) {
  if (missing(df)) {
    attributes(x)$survey_flow
  } else {
    attributes(df[[x]])$survey_flow
  }
}




# Create a vector containing character strings comprised of all the variable
# labels for each column in a data.frame or tibble.
# write a function that will get the variable label for each column in the df
get_all_var_labels <- function(df, unlist = TRUE) {
  # get a list of columns
  cols <- names(df)

  # write up a function that makes the string in the format we want
  string_fun <- function(x) {
    string <- attributes(df[[x]])$label
  }

  # iterate string_fun over each of the columns laid out earlier
  var_labels <- lapply(cols, string_fun) %>%
    # set the names of the objects in the list
    setNames(cols)

  if (isTRUE(unlist)) {
    # map string_fun over each of the columns laid out earlier
    var_labels %>% unlist()
  } else {
    var_labels
  }

}

# Create a list of vectors of the value labels of each variable in a data.frame
get_all_val_labels <- function(df) {
  # get a list of columns
  cols <- names(df)

  # write up a function that makes the string in the format we want
  string_fun <- function(x) {
    string <- attributes(df[[x]])$labels
  }

  # map string_fun over each of the columns laid out earlier
  lapply(cols, string_fun) %>%
    # add the names of the columns to the list objects
    setNames(cols)

}

# Create a list of vectors of the factor levels of each variable in a data.frame
get_all_factor_levels <- function(df) {

  # get a list of columns
  cols <- names(df)

  # write up a function that makes the string in the format we want
  string_fun <- function(x) {
    string <- attributes(df[[x]])$levels
  }

  # map string_fun over each of the columns laid out earlier
  lapply(cols, string_fun) %>%
    # add the names of the columns to the list objects
    setNames(cols)

}

# Create a list of vectors of the factor levels of each variable in a data.frame
get_all_survey_flow <- function(df) {

  # get a list of columns
  cols <- names(df)

  # write up a function that makes the string in the format we want
  string_fun <- function(x) {
    string <- attributes(df[[x]])$survey_flow
  }

  # map string_fun over each of the columns laid out earlier
  lapply(cols, string_fun) %>%
    # add the names of the columns to the list objects
    setNames(cols)

}

# Create a list of vectors of the factor levels of each variable in a data.frame
get_all_question_preface <- function(df) {

  # get a list of columns
  cols <- names(df)

  # write up a function that makes the string in the format we want
  string_fun <- function(x) {
    string <- attributes(df[[x]])$question_preface
  }

  # map string_fun over each of the columns laid out earlier
  lapply(cols, string_fun) %>%
    # add the names of the columns to the list objects
    setNames(cols)

}

# Create a list of vectors of the factor levels of each variable in a data.frame
get_all_note <- function(df) {

  # get a list of columns
  cols <- names(df)

  # write up a function that makes the string in the format we want
  string_fun <- function(x) {
    string <- attributes(df[[x]])$note
  }

  # map string_fun over each of the columns laid out earlier
  lapply(cols, string_fun) %>%
    # add the names of the columns to the list objects
    setNames(cols)

}

# Create a list of vectors of the factor levels of each variable in a data.frame
get_all_transformation <- function(df) {

  # get a list of columns
  cols <- names(df)

  # write up a function that makes the string in the format we want
  string_fun <- function(x) {
    string <- attributes(df[[x]])$transformation
  }

  # map string_fun over each of the columns laid out earlier
  lapply(cols, string_fun) %>%
    # add the names of the columns to the list objects
    setNames(cols)

}






