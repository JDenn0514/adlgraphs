#' Make proper percent labels
#'
#' This function is a wrapper around a very common data transformation that is
#' done every time we make a frequency plot. It changes the `x` variable by
#' multiplying it 100 and creates a new variable called `pct_lab` which is a 
#' string of the `x` variable but with a "%" symbol added.
#' 
#'
#' @param data A data frame or vector. Can be left blank if used during piping
#' @param x a variable we want to convert to a percentage. The value is `pct`
#'   by default and should always be pct.
#' @param decimals Number of decimal places the percent should be rounded to
#'
#' @returns The original data.frame found in `data` with two changes. The column
#'   called "pct" is multiplied by 100 and a new column is created called "pct_lab"
#'   that is the same as pct but with a "%" symbol at the end of it.
#' 
#' @examples
#' library(dplyr)
#' # get the frequencies of top andupdate 
#' test_data %>%
#'   get_freqs(top) %>% 
#'   pct_conv()
#'
#' @export
pct_conv <- function(data, x = pct, decimals = 1) {
  data %>% dplyr::mutate(
    pct = pct*100,
    pct_lab := make_percent(pct, scale = 1, decimals = decimals))
}



