#' Get variable label
#'
#' This function makes it easy to get the variable labels from either an
#' individual vector or a data frame. NOTE: it is not possible to set or modify
#' the variable labels with this function.
#'
#'
#' @param x A vector object, the name of a column in a `data.frame`, or an
#'   an actual `data.frame` object.
#' @param data A `data.frame` or `tibble` object. This should only be specified
#'   when `x` is only the name of a column in a `data.frame`.
#' @param unlist Logical. If `TRUE`, the default, returns a named vector. If
#'   `FALSE`, returns a list. This only works when `x` is a `data.frame`
#'
#' @returns If `x` is a variable or vector, a string containing the "label" 
#'   attribute, if one is present, is returned. If `x` is a `data.frame` then a
#'   named vector with the "label" attribute from each variable is returned.
#' 
#' @examples 
#' library(adlgraphs)
#' # create a random vector
#' x <- sample(c(0, 1), replace = TRUE, size = 10)
#' # add a question preface attribute
#' attr(x, "question_preface") <- "This is a question preface"
#' # check to see that there is a new attribute called `question_preface`
#' attributes(x)
#' # now get the question_preface
#' attr_question_preface(x)
#' 
#' # now let's create a realistic workflow with a data.frame --------
#' # create a fake dataset
#' df <- data.frame(
#'   x_1 = sample(c(0, 1), replace = TRUE, size = 10),
#'   x_2 = sample(c(0, 1), replace = TRUE, size = 10),
#'   x_3 = sample(c(0, 1), replace = TRUE, size = 10),
#'   x_4 = sample(c(0, 1), replace = TRUE, size = 10)
#' ) 
#' 
#' # set the variable labels
#' attr(df$x_1, "label") <- "Which of the following colors do you like? Blue"
#' attr(df$x_2, "label") <- "Which of the following colors do you like? Red"
#' attr(df$x_3, "label") <- "Which of the following colors do you like? Yellow"
#' attr(df$x_4, "label") <- "Which of the following colors do you like? Purple" 
#' 
#' # now let's check the variable labels
#' attr_var_label(df)
#' 
#' # this isn't an ideal variable label if we want to show how many people
#' # like each of the colors, so let's fix that 
#' 
#' # set the value labels (most survey data should have this)
#' attr(df$x_1, "labels") <- c("Blue" = 1)
#' attr(df$x_2, "labels") <- c("Red" = 1)
#' attr(df$x_3, "labels") <- c("Yellow" = 1)
#' attr(df$x_4, "labels") <- c("Purple" = 1)
#' 
#' # add the question prefaces and update the variable labels for each column in df
#' for(x in names(df)) {
#'   df[[x]] <- set_question_preface(x, df)
#' }
#' 
#' # now let's check the updated variable labels
#' attr_var_label(df)
#' 
#' @export
attr_var_label <- function(x, data, unlist) {
  UseMethod("attr_var_label")
}

#' @export
attr_var_label.default <- function(x, data, unlist = NULL) {
  if (missing(data)) {
    attr(x, "label", exact = TRUE)
  } else {
    attr(data[[x]], "label", exact = TRUE)
  }
}


# Create a vector containing character strings comprised of all the variable
# labels for each column in a data.frame or tibble.
# write a function that will get the variable label for each column in the data
#' @export
attr_var_label.data.frame <- function(x, data = NULL, unlist = TRUE) {
  # get a list of columns
  cols <- names(x)

  # write up a function that makes the string in the format we want
  string_fun <- function(var) {
    string <- attr(x[[var]], "label", exact = TRUE)
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






