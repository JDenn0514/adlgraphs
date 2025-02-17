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
#' @param group <[`tidy-select`][dplyr_tidy_select]> A selection of columns to 
#'   group the data by in addition to `treats`. This operates very similarly
#'   to `.by` from dplyr (for more info on that see [?dplyr_by][dplyr_by]). 
#'   See examples to see how it operates.
#' @param wt Can be either character strings or symbols. Weights. Add if 
#'   you have a weighting variable and want to get weighted correlations
#' @param decimals Number of decimals each number should be rounded to. Default
#'   is 3.
#'
#' @returns A tibble showing correlations (`correlation`), number of observations 
#'   (`n`), low and high confidence intervals (`conf.low`, `conf.high`), 
#'   the p-value (`p_value`), and stars indicating it's statistical significance.
#'   If data is of class `"grouped_df"` or the `group` argument is specified,
#'   it will return one row for each unique observation if one group is provided
#'   and one row per unique combination of observations if multiple groups are used.
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
get_corr <- function(
  data,
  x,
  y,
  group = NULL,
  wt = NULL,
  decimals = 3
) {

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

  if (is.null(group_names)) {
    # if group is null just get the normal correlation
    out <- wtd_corr(data, x = {{ x }}, y = {{ y }}, wt = {{ wt }}, decimals = decimals)

  } else {

    # make a nested data frame
    nest_data <- make_nested(data, {{ group_names }})

    # get the groups 
    # we will combine this with the correlations
    just_groups <- nest_data[c(group_names)]

    # make the correlation dataframe
    corr_df <- purrr::map(
      # we are iterating over the data column
      nest_data$data, 
      # use wtd_corr to get the individuals correlations
      ~wtd_corr(data = .x, x = {{ x }}, y = {{ y }}, wt = {{ wt }}, decimals = decimals) 
    ) %>% 
      # bind the rows together
      dplyr::bind_rows()

    # combine the columns of the grouping variable with the correlation
    out <- dplyr::bind_cols(just_groups, corr_df)
    # sort the first two columns
    out <- sort_by(out, out[group_names])

    # add the group labels
    # get the variable labels as a named list
    group_labels <- attr_var_label(data[,group_names])
    # for each value in names(group_labels) add the variable label from group_labels
    for (var in names(group_labels)) attr(out[[var]], "label") <- group_labels[[var]]
  
    
  }

  # add an attribute containing the names of the grouping variables
  attr(out, "group_names") <- group_names

  attr(out$correlation, "label") <- "Correlation"
  attr(out$n, "label") <- "N"
  attr(out$conf.low, "label") <- "Low CI"
  attr(out$conf.high, "label") <- "High CI"
  attr(out$p_value, "label") <- "P-Value"

  # get the classes of the data.frame
  class_names <- class(out)
  # add adlgraphs_freqs to the classes
  attr(out, "class") <- c("adlgraphs_means", class_names)

  out

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
#' @param decimals Number of decimals each number should be rounded to. Default
#'   is 3.
#' 
#' @export
wtd_corr <- function(data, x, y,  wt, decimals = 3) {

  x <- rlang::as_name(rlang::ensym(x))

  # Check if x is numeric
  if (!is.numeric(data[[x]])) {
    cli::cli_abort(c(
      "`{x}` must be a numeric variable.",
      x = "Supplied variable is {class(data[[x]])}."
    ))
  }

  y <- rlang::as_name(rlang::ensym(y))

  # Check if x is numeric
  if (!is.numeric(data[[y]])) {
    cli::cli_abort(c(
      "`{y}` must be a numeric variable.",
      x = "Supplied variable is {class(data[[y]])}."
    ))
  }

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
    data[[wt]] <- rep(1, length(data[[x]]))  
  } else {
    # ensure that string or symbol are accepted in wt
    wt <- rlang::as_name(rlang::ensym(wt))

    if (!is.numeric(data[[wt]])) {
      # if it is not numeric then return an error
      cli::cli_abort(c(
        "`{wt}` must be a numeric variable.",
        x = "Supplied variable is {class(data[[wt]])}."
      ))

    } else {
      # if it is numeric, replace NAs with 0
      data[[wt]][is.na(data[[wt]])] <- 0
    }

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

  # round decimals 
  round_cols <- c("correlation", "conf.low", "conf.high", "p_value")

  out[round_cols] <- lapply(
    round_cols,
    \(x) round(out[[x]], decimals)
  )

  attr(out$correlation, "label") <- "Correlation"
  attr(out$n, "label") <- "N"
  attr(out$conf.low, "label") <- "Low CI"
  attr(out$conf.high, "label") <- "High CI"
  attr(out$p_value, "label") <- "P-Value"

  class(out) <- c("tbl_df", "tbl", "data.frame")

  return(out)

}





