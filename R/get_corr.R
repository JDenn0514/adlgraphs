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
#' @returns A tibble showing correlations (`correlation`), number of observations 
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

  x <- adlgraphs:::accept_string_or_sym({{ x }})
  y <- adlgraphs:::accept_string_or_sym({{ y }})

  # get variable labels
  x_lab <- attr_var_label(data[[x]])
  y_lab <- attr_var_label(data[[y]])

  # get variable names
  x_name <- rlang::enexpr(x)
  y_name <- rlang::enexpr(y)

  # create the named vectors
  x_name_vec <- stats::setNames(x_name, x_lab)
  y_name_vec <- stats::setNames(y_name, y_lab)

  # Prepare weights
  if (missing(wt)) {
    wt <- "wts"
    data[[wt]] <- rep(1, nrow(data))
  } else {
    wt <- rlang::as_name(rlang::ensym(wt))
    data[[wt]][is.na(data[[wt]])] <- 0
  }

  data <- data[c(x, y, wt)]
  data <- data[stats::complete.cases(data),]

  # standardize the data
  data[[x]] <- stdz(data[[x]], data[[wt]])
  data[[y]] <- stdz(data[[y]], data[[wt]])

  model <- stats::lm(
    stats::reformulate(x, y),
    data = data,
    weights = data[[wt]]
  )
  
  coefs <- coef(summary(model))[2,]
  coefs <- as.data.frame(t(coefs))

  # get the length of x without NAs
  n <- length(data[[x]])
  # make a new column with the number of observations
  coefs$n <- n

  # calculate z
  z <- stats::qt(1 - ((1 - 0.95) / 2), df = model$df.residual)
  # calculate the margin of error
  coefs$moe <- z * coefs$`Std. Error`
  # calculate low confidence interval
  coefs$conf.low <- coefs$Estimate - coefs$moe
  # calculate high CI
  coefs$conf.high <- coefs$Estimate + coefs$moe
  # create a new column with the stars
  coefs$stars <- stars_pval(coefs[["Pr(>|t|)"]])

  coefs$x <- x_name
  if (!is.null(x_lab)) {
    # if x_lab is not null 

    # add labels
    attr(coefs$x, "labels") <- x_name_vec
    # update the class
    class(coefs$x) <- c("haven_labelled", "vctrs_vctr", "character")
  }
  # set the y column using the 
  coefs$y <- y_name
  if (!is.null(y_lab)) {
    # if y_lab is not null 

    # add labels
    attr(coefs$y, "labels") <- y_name_vec
    # update the class
    class(coefs$y) <- c("haven_labelled", "vctrs_vctr", "character")
  }

  out <- coefs[c("x", "y", "Estimate", "n", "conf.low", "conf.high", "Pr(>|t|)", "stars")]
  names(out) <- c("x", "y", "correlation", "n", "conf.low", "conf.high", "p_value", "stars")

  attr(out$correlation, "label") <- "Correlation"
  attr(out$n, "label") <- "N"
  attr(out$conf.low, "label") <- "Low CI"
  attr(out$conf.high, "label") <- "High CI"
  attr(out$p_value, "label") <- "P-Value"

  class(out) <- c("tbl_df", "tbl", "data.frame")

  return(out)

}


# standardize the data using weights
stdz <- function(x, wt = NULL){

  # Prepare weights
  if (missing(wt)) {
    wt <- rep(1, length(x))
  } 

  # calculate the weighted n
  n <- sum(wt, na.rm = TRUE)
  # center x by subtracting the mean from it
  x_mean <- x - (sum(x * wt, na.rm = TRUE) / n)
  # calculate the weighted sd
  x_sd <- sqrt(sum(wt * (x_mean)^2, na.rm = TRUE) / n)
  # divide x by the weighted sd of x to scale the data
  x <- x_mean / x_sd
  x


}


