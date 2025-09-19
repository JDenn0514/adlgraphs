#' Set a new attribute called `question_preface`
#'
#' This function adds a new attribute called \code{question_preface} based on
#' the variable label and value label. It is mostly for variables that come
#' from survey questions that allow for multiple selections (i.e., a
#' "Select all that apply" style question).
#'
#' @param x A vector object, the name of a column in a `data.frame`, or an
#'   an actual `data.frame` object.
#' @param data A `data.frame` or `tibble` object. This should only be specified
#'   when `x` is only the name of a column in a `data.frame`.
#' @param custom If a string is provided, sets the `question_preface` attribute
#'   manually. If nothing is provided, will use the existing variable label
#'   and value label by removing the value label from the variable label. See
#'   examples to see it in practice.
#' @param update_var_label Logical. If `TRUE`, uses the value label to set
#'   a new variable label.
#'
#' @return A vector the same type and length of x but with an additional attribute.
#'
#' @examples
#'
#' library(adlgraphs)
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
#' # set the value labels
#' attr(df$x_1, "labels") <- c("Blue" = 1)
#' attr(df$x_2, "labels") <- c("Red" = 1)
#' attr(df$x_3, "labels") <- c("Yellow" = 1)
#' attr(df$x_4, "labels") <- c("Purple" = 1)
#'
#' # now let's add a question_preface attribute
#' df$x_1 <- set_question_preface(df$x_1)
#' # check to make sure it showed up
#' attributes(df$x_1)
#' # now let's not update the variable label
#' df$x_2 <- set_question_preface(df$x_2, update_var_label = FALSE)
#' # check the attributes
#' attributes(df$x_2)
#' # can also separate the variable and data like so
#' df$x_3 <- set_question_preface("x_3", data = df)
#' # check the attributes
#' attributes(df$x_3)
#' # add a custom string
#' df$x_4 <- set_question_preface(df$x_4, custom = "Colors liked")
#' # check the attributes
#' attributes(df$x_4)
#'
#' @export
set_question_preface <- function(x, data, custom, update_var_label = TRUE) {
  if (!missing(data)) {
    x <- data[[x]]
  }

  if (!missing(custom)) {
    attr(x, "question_preface") <- custom
  } else {
    # get the value label
    string <- paste0(" ", names(attr_val_labels(x)))
    matches <- attr_var_label(x)
    if (grepl(" - Selected Choice", matches)) {
      matches <- sub(" - Selected Choice", "", matches, fixed = TRUE)
    }
    attr(x, "question_preface") <- gsub(string, "", matches, fixed = TRUE)
  }

  if (update_var_label) {
    attr(x, "label") <- names(attr_val_labels(x))
  }
  return(x)
}
