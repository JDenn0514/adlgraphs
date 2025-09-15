#' Get the frequencies as a GT table
#'
#' This is essentially a wrapper around `get_freqs()` and `prettytable()` so you 
#' only have to call this rather than calling both functions. It takes the 
#' `get_freqs()` output and then makes it pretty using `prettytable()`.
#'
#' @param data An object of type data.frame or tibble. If piping the data into the
#'   function, this is not required.
#' @param x Either a character string or symbol. The variable with which want
#'   to get the frequencies.
#' @param group <[`tidy-select`][dplyr_tidy_select]> A selection of columns to 
#'   group the data by in addition to `treats`. This operates very similarly
#'   to `.by` from dplyr (for more info on that see [?dplyr_by][dplyr_by]). 
#' @param wt Weights. Add if you have a weighting variable and want to get
#'   weighted frequencies.
#' @param drop_zero Logical. Determines if rows with 0 should be removed 
#'   Default is `FALSE`.
#' @param decimals Number of decimals each number should be rounded to. Default
#'   is 3.
#' @param na.rm Logical. Determines if NAs should be kept or removed Default is
#'   `TRUE`.
#' @param show_genpop Logical. If the data is grouped, determines if data should 
#'   should be shown for the general population as well. `FALSE`, the default, 
#'   does not show the results for the general population. `TRUE` shows the 
#'   results for the general population in a new column.
#'
#' @export
get_freq_table <- function(
  data, 
  x, 
  group = NULL, 
  wt = NULL, 
  drop_zero = FALSE,
  decimals = 3,
  na.rm = TRUE,
  show_genpop = FALSE
) {

  x <- rlang::enexpr(x)
  group <- rlang::enexpr(group)
  wt <- rlang::enexpr(wt)

  
  freqs <- get_freqs(
    data = data, 
    x = {{ x }}, 
    group = {{ group }}, 
    wt = {{ wt }}, 
    drop_zero = drop_zero,
    decimals = decimals,
    na.rm = na.rm
  ) %>% 
    prettytable(show_genpop = show_genpop)

  freqs
}
