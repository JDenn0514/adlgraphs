#' Get correlations for a combination of variables
#' 
#' `get_all_corr` makes it easy to calculate correlations across 
#' every variable in a data frame or a select set of variables. It also
#' works with grouped data frames so you can check correlations among
#' the levels of several grouping variables.
#' 
#' @param data A data frame or tibble object
#' @param cols  <[`tidy-select`][dplyr_tidy_select]> The variables you want 
#'   to get the correlations for. 
#' @param group <[`tidy-select`][dplyr_tidy_select]> A selection of columns to 
#'   group the data by in addition to `treats`. This operates very similarly
#'   to `.by` from dplyr (for more info on that see [?dplyr_by][dplyr_by]). 
#'   See examples to see how it operates.
#' @param wt A variable to use as the weights for weighted correlations
#' @param remove_redundant Should rows where the two variables are the same be 
#'   kept or removed? If `TRUE`, the default, they are removed. 
#' @param decimals Number of decimals each number should be rounded to. Default
#'   is 3.
#' 
#' @returns A data.frame with the correlations between every combination of 
#'   columns in `data`.
#' 
#' 
#' @examples
#' # load dplyr and adlgraphs
#' library(dplyr)
#' library(adlgraphs)
#' 
#' # To get correlations with three variables you can do it three ways
#' # 1. Create a new data frame with only the columns you want
#' new_data <- test_data %>% dplyr::select(top:dominate)
#' get_all_corr(new_data)
#' 
#' # 2. Using dplyr::select() and pipes
#' test_data %>% 
#'   dplyr::select(c(top:dominate)) %>% 
#'   get_all_corr()
#' 
#' # 3. Use the `cols` argument
#' get_all_corr(test_data, cols = c(top:dominate))
#' # or 
#' test_data %>% get_all_corr(c(top:dominate))
#' 
#' # To get weighted correlations just specify the `wt` argument
#' test_data %>% get_all_corr(c(top:dominate), wt = wts)
#' 
#' # You can also calculate grouped correlations. For example, if
#' # you were interested in comparing the weighted correlations 
#' # among people with a college degree vs those without one, you 
#' # would do it like this:
#' test_data %>% 
#'   dplyr::group_by(edu_f2) %>% 
#'   get_all_corr(c(top:dominate), wt = wts)
#' 
#' # Another way to calculate grouped correlations is to 
#' # specify the group argument inside the function call:
#' get_all_corr(test_data, c(top:dominate), edu_f2, wts)
#' 
#' # You can also use multiple grouping variables
#' get_all_corr(test_data, c(top:dominate), c(edu_f2, pid_f3), wts)
#' 
#' 
#' @export
get_all_corr <- function(data, cols, group, wt, remove_redundant = TRUE, decimals = 3) {

  # Prepare weights
  if (missing(wt)) {
    wt <- "wts"
    data[[wt]] <- rep(1, nrow(data))
  } else {
    wt <- rlang::as_name(rlang::ensym(wt))
    data[[wt]][is.na(data[[wt]])] <- 0
  }

  # Prepare group variables
  # if the data is grouped, use dplyr::group_vars to get them, else set to NULL
  group_names <- if(inherits(data, "grouped_df")) dplyr::group_vars(data) else NULL
  # if group arg is missing set to NULL, else use as.character(substitute()) to capture it
  group_vars <- if (missing(group)) NULL else select_groups({{ group }}, data)
  # remove the "c" from the group_vars vector if it is there
  group_vars <- group_vars[group_vars != "c"]
  # combine group_names and group_vars for the final vector of group names
  # use unique to make sure there aren't any duplicates
  group_names <- unique(c(group_names, group_vars))

  if (!missing(cols) & !is.null(group_names)) {
    data <- dplyr::select(data, {{ cols }}, tidyselect::all_of(c(group_names, wt)))
  } else if (!missing(cols) & is.null(group_names)) {
    data <- dplyr::select(data, {{ cols }}, tidyselect::all_of(wt))
  }

  # get all of the variables that are numeric
  num_data <- data %>% 
    dplyr::ungroup() %>% 
    dplyr::select(tidyselect::where(is.numeric))

  # get a combination of every variable
  combinations <- expand.grid(names(num_data[names(num_data) != wt]), names(num_data[names(num_data) != wt]), stringsAsFactors = FALSE)
  
  if (isTRUE(remove_redundant)) {
    combinations <- dplyr::filter(combinations, !Var1 == Var2)
  }

  # create a simple internal function to calculate the correlations
  internal_corr <- function(i) {

    # get the variable names
    x_name <- combinations[i, "Var1"]
    y_name <- combinations[i, "Var2"]

    if (!is.null(group_names)) {
      get_corr(data, {{ x_name }}, {{ y_name }}, group = {{ group_names }}, wt = {{ wt }}, decimals = decimals)
    } else {
      wtd_corr(data, {{ x_name }}, {{ y_name }}, wt = {{ wt }}, decimals = decimals)
    }

  }

  # get all of the correlations
  purrr::map(
    seq_len(nrow(combinations)), 
    internal_corr
  ) %>% 
    # combine into a data frame
    dplyr::bind_rows()

}
