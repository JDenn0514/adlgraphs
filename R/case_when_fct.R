#' `case_when` with factor levels
#'
#' Recode a variable using the `dplyr::case_match()` syntax
#'
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas. The left hand side (LHS) determines which values match this case.
#'   The right hand side (RHS) provides the replacement value.
#'
#'   The LHS inputs must evaluate to logical vectors.
#'
#'   The RHS inputs will be coerced to their common type.
#'
#'   All inputs will be recycled to their common size. That said, we encourage
#'   all LHS inputs to be the same size. Recycling is mainly useful for RHS
#'   inputs, where you might supply a size 1 input that will be recycled to the
#'   size of the LHS inputs.
#'
#'   `NULL` inputs are ignored.
#'
#' @param .default The value used when all of the LHS inputs return either
#'   `FALSE` or `NA`.
#'
#'   `.default` must be size 1 or the same size as the common size computed
#'   from `...`.
#'
#'   `.default` participates in the computation of the common type with the RHS
#'   inputs.
#'
#'   `NA` values in the LHS conditions are treated like `FALSE`, meaning that
#'   the result at those locations will be assigned the `.default` value. To
#'   handle missing values in the conditions differently, you must explicitly
#'   catch them with another condition before they fall through to the
#'   `.default`. This typically involves some variation of `is.na(x) ~ value`
#'   tailored to your usage of `case_when()`.
#'
#'   If `NULL`, the default, a missing value will be used.
#'
#' @export

case_when_fct <- function(..., .default = NULL) {
  # get the arguments from ...
  args <- rlang::list2(...)
  # get the length of the arguments + 1
  arg_len <- length(args) + 1

  # extract right hand side of formula
  levels <- purrr::map(args, rlang::f_rhs)
  # remove levels with NA
  levels <- levels[!is.na(levels)]

  # add .default to the levels
  # this makes sure if you use .default instead of TRUE ~, it will return the
  # proper vector instead of NAs
  levels[[arg_len]] = .default

  # set the vector to a factor and specify the levels
  factor(
    dplyr::case_when(..., .default = .default),
    levels = levels
  )

}





