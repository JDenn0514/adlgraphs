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

pct_conv <- function(data, x = pct) {
  data %>%
    dplyr::mutate(
      pct := round({{ x }} * 100, 1),
      pct_lab := scales::percent(pct, scale = 1)
    )
}
