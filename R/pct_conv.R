#' Make proper percent labels
#'
#' This function is a wrapper around a very common data transformation that is
#' done every time we make a frequency plot
#'
#' @param data A data frame or vector. Can be left blank if used during piping
#' @param x a variable we want to convert to a percentage. The value is `pct`
#'   by default and should always be pct.
#'
#' @export
#'
#' @examples
#'
#' library(tibble)
#' library(dplyr)
#' library(labelled)
#' library(haven)
#'
#' # create the fake data
#' df <- tibble::tribble(
#'   ~x, ~y, ~z,
#'   3, 2, 3,
#'   4, 4, 2,
#'   2, 6, 1,
#'   1, 1, 4,
#'   5, 4, 3,
#'   6, 5, 6
#' )
#'
#' df %>%
#'   count(x) %>%
#'   mutate(pct = prop.table(n)) %>%
#'   pct_conv()
#'
#'

pct_conv <- function(data, x = pct) {
  data %>%
    dplyr::mutate(
      pct := round({{ x }} * 100, 1),
      pct_lab := scales::percent(pct, scale = 1)
    )
}


df <- tibble::tribble(
  ~x, ~y, ~z,
  3, 2, 3,
  4, 4, 2,
  2, 6, 1,
  1, 1, 4,
  5, 4, 3,
  6, 5, 6
)


df %>%
  count(x) %>%
  mutate(pct = prop.table(n)) %>%
  pct_conv()
