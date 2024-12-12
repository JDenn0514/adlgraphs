#' `case_match` with factor levels
#'
#' Recode a variable using the `dplyr::case_match()` syntax
#'
#' @param .x A vector to match against.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas: `old_values ~ new_value`. The right hand side (RHS) determines
#'   the output value for all values of `.x` that match the left hand side
#'   (LHS).
#'
#'   The LHS must evaluate to the same type of vector as `.x`. It can be any
#'   length, allowing you to map multiple `.x` values to the same RHS value.
#'   If a value is repeated in the LHS, i.e. a value in `.x` matches to
#'   multiple cases, the first match is used.
#'
#'   The RHS inputs will be coerced to their common type. Each RHS input will be
#'   [recycled][vctrs::theory-faq-recycling] to the size of `.x`.
#'
#' @param .default The value used when values in `.x` aren't matched by any of
#'   the LHS inputs. If `NULL`, the default, a `NA` will be used.
#'
#' @return  A factor vector with the same size as `.x` and the same type as the
#'   common type of the RHS inputs and `.default` and levels as defined by the 
#'   order of the RHS inputs.
#' 
#' @seealso [case_when_fct()]
#'
#' @examples
#' # import dplyr so we can use their starwars dataset
#' library(dplyr)
#' # create a vector with a variable label
#' species <- starwars$species %>% structure(label = "This is a variable label")
#'
#' new_species <- case_match_fct(
#'   species,
#'   "Human" ~ "Humanoid",
#'   "Droid" ~ "Robot",
#'   c("Wookiee", "Ewok") ~ "Hairy",
#'   .default = "Other"
#' )
#'
#' # now let's check to see that it added the transformation metadata and the
#' #variable label
#' str(new_species)
#'
#' # now let's create a variable "new_species" and get the frequencies for it
#' # we can see the frequencies are in the same order we applied.
#' starwars %>%
#'   dplyr::mutate(
#'     new_species = case_match_fct(
#'       species,
#'       "Human" ~ "Humanoid",
#'       "Droid" ~ "Robot",
#'       c("Wookiee", "Ewok") ~ "Hairy",
#'       .default = "Other"
#'     )
#'   ) %>%
#'   dplyr::count(new_species)
#'
#' # now let's do the same but with dplyr::case_match()
#' # we can see that the frequencies are in alphabetical order
#' starwars %>%
#'   dplyr::mutate(
#'     new_species = dplyr::case_match(
#'       species,
#'       "Human" ~ "Humanoid",
#'       "Droid" ~ "Robot",
#'       c("Wookiee", "Ewok") ~ "Hairy",
#'       .default = "Other"
#'     )
#'   ) %>%
#'   dplyr::count(new_species)
#' 
#' @export
case_match_fct <- function(.x, ..., .default = NULL) {

  # get the object's name
  x_lab <- deparse(substitute(.x))

  # get the arguments from ...
  args <- rlang::list2(...)

  # get the length of the arguments + 1
  arg_len <- length(args) + 1

  seq_arg_len <- seq_len(length(args))

  # extract left-hand side of formula
  args_lhs <- purrr::map(args, rlang::f_lhs)

  # extract right hand side of formula
  args_rhs <- purrr::map(args, rlang::f_rhs)
  # remove levels with NA
  args_rhs <- args_rhs[!is.na(args_rhs)]

  # create the function
  text_fun <- function(x) {
    glue::glue("What was '{args_lhs[x]}' has become '{args_rhs[x]}'")
  }
  # create the main text that goes in the transformation
  text <- purrr::map(seq_arg_len, text_fun) %>%
    purrr::list_c() %>%
    paste(collapse = "\n")


  # add .default to the levels
  # this makes sure if you use .default instead of TRUE ~, it will return the
  # proper vector instead of NAs
  args_rhs[[arg_len]] = .default

  default <- .default

  if (is.null(.default)) {
      # set the vector to a factor and specify the levels
    factor(
      dplyr::case_match(.x, ..., .default = .default),
      levels = args_rhs
    ) %>%
      structure(
        label = attr_var_label(.x),
        transformation = glue::glue(
          "Recoded '{x_lab}' as a factor and set the levels based on their order.
          The data transformation is as follows:
          {text}"
        )
      )

  } else {
    # set the vector to a factor and specify the levels
    factor(
      dplyr::case_match(.x, ..., .default = .default),
      levels = args_rhs
    ) %>%
      structure(
        label = attr_var_label(.x),
        transformation = glue::glue(
          "Recoded '{x_lab}' as a factor and set the levels based on their order.
        The data transformation is as follows:
        {text},
        Everything else has become '{default}'"
        )
      )

  }

}










