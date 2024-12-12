#' Get correlations for a combination of variables
#' 
#' `get_all_corr` makes it easy to calculate correlations across 
#' every variable in a data frame or select set of variables. It also
#' works with grouped data frames so you can check correlations among
#' the levels of several grouping variables.
#' 
#' @param data A data frame or tibble object
#' @param cols  <[`tidy-select`][dplyr_tidy_select]> The variables you want 
#'   to get the correlations for. 
#' @param wt A variable to use as the weights for weighted correlations
#' @param remove_redundant Should rows where the two variables are the same be 
#'   kept or removed? If `TRUE`, the default, they are removed. 
#' 
#' @return A data.frame with the correlations between every combination of 
#'   columns in `data`.
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
#' 
#' 
#' @export
get_all_corr <- function(data, cols, wt = NULL, remove_redundant = TRUE) {
  UseMethod("get_all_corr")
}

#' @export
get_all_corr.default <- function(data, cols, wt = NULL, remove_redundant = TRUE) {

  if (!missing(cols)) {
    data <- dplyr::select(data, {{ cols }})
  }

  data <- data %>% 
    dplyr::ungroup() %>% 
    dplyr::select(tidyselect::where(is.numeric))

  # get a combination of every variable
  combinations <- expand.grid(names(data), names(data), stringsAsFactors = FALSE)
  
  if (isTRUE(remove_redundant)) {
    combinations <- dplyr::filter(combinations, !Var1 == Var2)
  }

  internal_corr <- function(i) {

    # get the variable names
    x_name <- as.character(combinations[i, "Var1"])
    y_name <- as.character(combinations[i, "Var2"])

    # run the correlation
    corr <- wtd_corr({{ data }}, {{ x_name }}, {{ y_name }}, wt = wt)
    # set the value labels for 
    return(corr)

  }

  # get all of the correlations
  purrr::map(
    seq_len(nrow(combinations)), 
    internal_corr
  ) %>% 
    # combine into a data frame
    dplyr::bind_rows()

}

#' @export
get_all_corr.grouped_df <- function(data, cols, wt = NULL, remove_redundant = TRUE) {

  group_helpers <- group_analysis_helper(data = data, cols = {{ cols }})

  # make the correlation dataframe
  corr_df <- purrr::map(
    # we are iterating over the data column
    group_helpers$nest_data$data, 
    # use get_all_corr.data.frame to get the individuals loadings
    ~get_all_corr.default(
      data = .x, 
      wt = wt,
      remove_redundant = remove_redundant
    )
  ) 

   # name the objects in the list
   names(corr_df) <- group_helpers$just_groups

   if (length(group_helpers$nest_data) > 2) {
     # if there are two or more grouping variables do the following
 
     # create the output data frame
     # combine the list objects and make a new variable 
     # called "groups" containing the names of each list
     out <- dplyr::bind_rows(corr_df, .id = "groups") %>% 
       # split up the string by the - and use the group_labs vector as the names
       tidyr::separate_wider_delim(groups, delim = " - ", names = c(group_helpers$group_labs)) %>% 
       # group the data by the vector group variables
       dplyr::group_by(dplyr::across(tidyselect::all_of(group_helpers$group_labs))) %>% 
       # arrange by the groups
       dplyr::arrange(.by_group = TRUE)
     
   } else {
 
     # create the output data frame
     # combine the list objects and make a new variable 
     # called "groups" containing the names of each list
     out <- dplyr::bind_rows(corr_df, .id = group_helpers$group_labs) %>% 
       # arrange by the groups
       dplyr::arrange(.by_group = TRUE) 
 
   }
  
  return(out)
 
}

