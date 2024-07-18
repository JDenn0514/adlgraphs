#' #' Functions to access metadata
#' #'
#' #' These are a set of functions that make it easy to access attributes/metadata
#' #' from a vector or column in a `data.frame`. Note: these functions do not allow
#' #' you to change, set, or remove attributes.
#' #'
#' #' @param x A vector object or the name of a column in a data.frame
#' #' @param df A `data.frame` or `tibble` object. This should be specified when `x`
#' #'   is only the name of a column.
#' #'
#' #' @details
#' #' \describe{
#' #'
#'
#' #'
#' #' \item{`attr_levels()`}{This function gets the factor levels.}
#' #'
#' #' \item{`attr_transformation()`}{This function gets the "transformation"
#' #' attribute which tells you what data transformation the variable underwent
#' #' when it was created.}
#' #'
#' #' \item{`attr_question_preface()`}{This function gets the "question_preface"
#' #' attribute. Some questions in surveys enable respondents to select multiple
#' #' responses. Each response gets it's own variable in the data, which is labeled
#' #' "label. The text stored in this attribute contains the text that respondents
#' #' saw before the response options.}
#' #'
#' #' \item{`attr_survey_flow()`}{This function gets teh the "survey_flow"
#' #' attribute. This is used to indicate if there was an experiment or some sort
#' #' of branching involved in the survey flow}
#' #'
#' #' \item{`attr_note()`}{This function gets the  "note" attribute which
#' #' contains miscellaneous information about the variable.}
#' #'
#' #'}
#' #'
#' #' @examples
#' #' library(adlgraphs)
#' #'
#' #'
#' #' # get the levels for edu_f
#' #' attr_levels(x = test_data$edu_f)
#' #' attr_levels(x = "edu_f", df = test_data)
#' #'
#' #' # get the transformation
#' #' attr_transformation(x = test_data$edu_f)
#' #' attr_transformation(x = "edu_f", df = test_data)
#' #'
#' #'
#' #' @name attributes
#' #' @aliases NULL
#' NULL
#'
#'
#'
#'
#'
#' #' Get the factor levels attribute from a vector
#' #' @rdname attributes
#' #' @export
#' attr_levels <- function(x, df) {
#'   if (missing(df)) {
#'     attributes(x)$levels
#'   } else {
#'     attributes(df[[x]])$levels
#'   }
#' }
#'
#' #' Get the transformation attribute from a vector
#' #' @rdname attributes
#' #' @export
#' attr_transformation <- function(x, df) {
#'   if (missing(df)) {
#'     attributes(x)$transformation
#'   } else {
#'     attributes(df[[x]])$transformation
#'   }
#' }
#'
#' #' Get the note attribute
#' #' @rdname attributes
#' #' @export
#' attr_note <- function(x, df) {
#'   if (missing(df)) {
#'     attributes(x)$note
#'   } else {
#'     attributes(df[[x]])$note
#'   }
#' }
#'
#' #' get question preface attribute
#' #' @rdname attributes
#' #' @export
#' attr_question_preface <- function(x, df) {
#'   if (missing(df)) {
#'     attributes(x)$question_preface
#'   } else {
#'     attributes(df[[x]])$question_preface
#'   }
#' }
#'
#'
#' # get survey_flow attribute
#' #' @rdname attributes
#' #' @export
#' attr_survey_flow <- function(x, df) {
#'   if (missing(df)) {
#'     attributes(x)$survey_flow
#'   } else {
#'     attributes(df[[x]])$survey_flow
#'   }
#' }
#'
#'
#'
#'
#'
#'
#' # Create a list of vectors of the factor levels of each variable in a data.frame
#' get_all_factor_levels <- function(df) {
#'
#'   # get a list of columns
#'   cols <- names(df)
#'
#'   # write up a function that makes the string in the format we want
#'   string_fun <- function(x) {
#'     string <- attributes(df[[x]])$levels
#'   }
#'
#'   # map string_fun over each of the columns laid out earlier
#'   lapply(cols, string_fun) %>%
#'     # add the names of the columns to the list objects
#'     setNames(cols)
#'
#' }
#'
#' # Create a list of vectors of the factor levels of each variable in a data.frame
#' get_all_survey_flow <- function(df) {
#'
#'   # get a list of columns
#'   cols <- names(df)
#'
#'   # write up a function that makes the string in the format we want
#'   string_fun <- function(x) {
#'     string <- attributes(df[[x]])$survey_flow
#'   }
#'
#'   # map string_fun over each of the columns laid out earlier
#'   lapply(cols, string_fun) %>%
#'     # add the names of the columns to the list objects
#'     setNames(cols)
#'
#' }
#'
#' # Create a list of vectors of the factor levels of each variable in a data.frame
#' get_all_question_preface <- function(df) {
#'
#'   # get a list of columns
#'   cols <- names(df)
#'
#'   # write up a function that makes the string in the format we want
#'   string_fun <- function(x) {
#'     string <- attributes(df[[x]])$question_preface
#'   }
#'
#'   # map string_fun over each of the columns laid out earlier
#'   lapply(cols, string_fun) %>%
#'     # add the names of the columns to the list objects
#'     setNames(cols)
#'
#' }
#'
#' # Create a list of vectors of the factor levels of each variable in a data.frame
#' get_all_note <- function(df) {
#'
#'   # get a list of columns
#'   cols <- names(df)
#'
#'   # write up a function that makes the string in the format we want
#'   string_fun <- function(x) {
#'     string <- attributes(df[[x]])$note
#'   }
#'
#'   # map string_fun over each of the columns laid out earlier
#'   lapply(cols, string_fun) %>%
#'     # add the names of the columns to the list objects
#'     setNames(cols)
#'
#' }
#'
#' # Create a list of vectors of the factor levels of each variable in a data.frame
#' get_all_transformation <- function(df) {
#'
#'   # get a list of columns
#'   cols <- names(df)
#'
#'   # write up a function that makes the string in the format we want
#'   string_fun <- function(x) {
#'     string <- attributes(df[[x]])$transformation
#'   }
#'
#'   # map string_fun over each of the columns laid out earlier
#'   lapply(cols, string_fun) %>%
#'     # add the names of the columns to the list objects
#'     setNames(cols)
#'
#' }
#'
#'
#'
#'
#'
#'
