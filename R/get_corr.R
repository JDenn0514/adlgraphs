#' Calculate weighted correlations
#' 
#' This function calculates weighted Pearson correlations between two variables. 
#' It also allows you to group the data and calculate correlations along each 
#' level of the grouping variable. If data is not grouped and no group is 
#' specified, then it will return the same output as [wtd_corr()].
#'
#' @param data An object of type data.frame or tibble. If piping the data into the
#'   function, this is not required.
#' @param x,y Can be either character strings or symbols. Name of two variables
#'   in the data you want to calculate the correlation between. 
#' @param group Can be either a character string or a symbol. The grouping 
#'   variable.
#' @param wt Can be either character strings or symbols. Weights. Add if 
#'   you have a weighting variable and want to get weighted correlations
#'
#' @return A tibble showing correlations (`correlation`), number of observations 
#'   (`n`), low and high confidence intervals (`conf.low`, `conf.high`), 
#'   the p-value (`p.value`), and stars indicating it's statistical significance.
#'   If data is of class `"grouped_df"` it will return one row for each unique 
#'   observation if one group is provided and one row per unique combination of 
#'   observations if multiple groups are used.
#' 
#' @examples
#' # load the dplyr for piping and grouping
#' library(dplyr)
#' 
#' # Let's first do a simple correlation where we pipe in the data
#' test_data %>% get_corr(x = top, y = sdo_sum)
#' 
#' # Repeat but with weights
#' test_data %>% get_corr(x = top, y = sdo_sum, wt = wts)
#' 
#' # Now let's get the correlatoin among only people with a bachelor's degree
#' test_data %>% 
#'   filter(edu_f2 == "At Least a Bachelor's Degree") %>% 
#'   get_corr(x = top, y = sdo_sum, wt = wts)
#' 
#' # Now let's get it for each education level. Two ways of doing this:
#' # The first is to group the data ahead of time
#' test_data %>% 
#'   group_by(edu_f) %>% 
#'   get_corr(x = top, y = sdo_sum, wt = wts)
#' 
#' # The second is to use the group argument
#' test_data %>% get_corr(x = top, y = sdo_sum, group = edu_f, wt = wts)
#' 
#' @export
get_corr <- function(data, x, y, group = NULL, wt) {
  UseMethod("get_corr")
}

#' @export
get_corr.default <- function(data, x, y, group, wt) {

  if (missing(group)) {
    # if group is null just get the normal correlation
    out <- wtd_corr(data, x = {{ x }}, y = {{ y }}, wt = {{ wt }})

  } else {

    group <- accept_string_or_sym({{ group }})
    # make a nested data frame
    nest_data <- data %>% 
      dplyr::group_by(.data[[group]]) %>% 
      tidyr::nest()

    # get the groups 
    # we will combine this with the correlations
    just_groups <- nest_data %>% dplyr::select(-data)

    # make the correlation dataframe
    corr_df <- purrr::map(
      # we are iterating over the data column
      nest_data$data, 
      # use wtd_corr to get the individuals correlations
      ~wtd_corr(data = .x, x = {{ x }}, y = {{ y }}, wt = {{ wt }}) 
    ) %>% 
      # bind the rows together
      dplyr::bind_rows()

    # combine the columns of the grouping variable with the correlation
    out <- dplyr::bind_cols(just_groups, corr_df)
    # sort the first two columns
    out <- out %>% 
      dplyr::arrange(.by_group = TRUE)

  }


  attr(out$correlation, "label") <- "Correlation"
  attr(out$n, "label") <- "N"
  attr(out$conf.low, "label") <- "Low CI"
  attr(out$conf.high, "label") <- "High CI"
  attr(out$p.value, "label") <- "P-Value"

  return(out)

}

#' @export
get_corr.grouped_df <- function(data, x, y, group, wt) {
  
  # make it a nested data set
  nest_data <- data %>% tidyr::nest()

  # get the groups 
  # we will combine this with the correlations
  just_groups <- nest_data %>% dplyr::select(-data)

  # make the correlation dataframe
  corr_df <- purrr::map(
    # we are iterating over the data column
    nest_data$data, 
    # use wtd_corr to get the individuals correlations
    ~wtd_corr(data = .x, x = {{ x }}, y = {{ y }}, wt = {{ wt }}) 
  ) %>% 
    # bind the rows together
    dplyr::bind_rows()
  
  # combine the columns of the grouping variable with the correlation
  out <- dplyr::bind_cols(just_groups, corr_df)
  # sort the first two columns
  out <- out %>% 
    dplyr::arrange(.by_group = TRUE)

  attr(out$correlation, "label") <- "Correlation"
  attr(out$n, "label") <- "N"
  attr(out$conf.low, "label") <- "Low CI"
  attr(out$conf.high, "label") <- "High CI"
  attr(out$p.value, "label") <- "P-Value"

  return(out)

}

#' Calculate individual weighted correlations
#' 
#' This is one of the main worker functions behind [get_corr()]. It calculates
#' weighted correlations and outputs the data as a one row tibble.
#' 
#' @param data An object of type data.frame or tibble. If piping the data into the
#'   function, this is not required.
#' @param x,y Can be either character strings or symbols. Name of two variables
#'   in the data you want to calculate the correlation between. 
#' @param wt Can be either character strings or symbols. Weights. Add if 
#'   you have a weighting variable and want to get weighted correlations
#' 
#' @export
wtd_corr <- function(data, x, y,  wt) {
  x <- accept_string_or_sym({{ x }})
  y <- accept_string_or_sym({{ y }})

  # get variable labels
  x_lab <- attr_var_label(data[[x]])
  y_lab <- attr_var_label(data[[y]])

  # get variable names
  x_name <- rlang::enexpr(x)
  y_name <- rlang::enexpr(y)

  # create the named vectors
  x_name_vec <- stats::setNames(x_name, x_lab)
  y_name_vec <- stats::setNames(y_name, y_lab)

  if (!missing(wt)) {
    # if not missing wt

    # make it accepting a string or symbol
    wt <- accept_string_or_sym({{ wt }})

    # get the correlation
    cor <- onecor.wtd(data[[x]], data[[y]], data[[wt]]) 

  } else {
    # if wt is missing then just calculate it without weights
    cor <- onecor.wtd(data[[x]], data[[y]])

  }

  out <- cor %>% 
    # converts the vector to a dataframe so we can use mutate
    tibble::enframe() %>% 
    # enframe makes it vertical so we need to pivot it to be wide again
    tidyr::pivot_wider(names_from = "name", values_from = "value") %>% 
    # add a bunch of variables and clean up data
    dplyr::mutate(
      # calculate the lower CI
      conf.low = correlation - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.err,
      # calculate the higher CI
      conf.high = correlation + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.err,
      # create the stars column based on p-value
      stars = stars_pval(p.value),
      # create the x variable and add value labels based on original variable label
      x = x_name %>% haven::labelled(labels = x_name_vec),
      # create the x variable and add value labels based on original variable label
      y = y_name %>% haven::labelled(labels = y_name_vec)
    ) %>% 
    # reorder the columns and drop some
    dplyr::select(c(x, y, correlation, n, conf.low, conf.high, p.value, stars))

  attr(out$correlation, "label") <- "Correlation"
  attr(out$n, "label") <- "N"
  attr(out$conf.low, "label") <- "Low CI"
  attr(out$conf.high, "label") <- "High CI"
  attr(out$p.value, "label") <- "P-Value"

  return(out)

}


# Calculate the weighted correlations as a matrix
onecor.wtd <- function(x, y, wt = NULL){
  if(is.null(wt)){
    wt <- rep(1, length(x))
  }
  # remove NAs
  use <- !is.na(y) & !is.na(x)
  # drop NAs
  x <- x[use]
  y <- y[use]
  wt <- wt[use]
  
  # get the length of x without NAs
  n <- length(x)

  # standardize the x and y values
  x_stdz <- stdz(x, wt = wt)
  y_stdz <- stdz(y, wt = wt)

  # get the correlation coefficients
  corcoef <- coef(summary(lm(y_stdz ~ x_stdz, weights=wt)))[2,]

  # clean up the final vector
  corcoef <- corcoef %>%
    # add the number of observations 
    append(n) %>% 
    # set the names
    setNames(c("correlation", "std.err", "t.value", "p.value", "n")) 

  return(corcoef)
}

# standardize the data using weights
stdz <- function(x, wt = NULL){

  if (is.null(wt)) {
    # if wt is null 
    x <- x - mean(x, na.rm = TRUE)
    x <- x / sd(x, na.rm = TRUE)
    return(x)
  } else {
    # subtract the weighted mean from x to center the data
    x <- x - weighted.mean(x, wt, na.rm = TRUE)
    # divide x by the weighted sd of x to scale the data
    x <- x / wtd_sd(x, wt, na.rm = TRUE)
    x
  }

}


