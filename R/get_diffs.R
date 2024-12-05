#' Calculate difference in means
#' 
#' @description
#' This function calculates the difference in means using a 
#' bivariate regression, as well the p-value indicating
#' how significant each difference is. The main function doing the
#' calculations `lm()`.
#' 
#' NOTE: This function does not perform an actual Dunnet Test as it 
#' does not calculate the quantile of the multivariate t-distribution
#' when determining the confidence intervals and p-values. If you need
#' to perform an actual Dunnett Test use the `dunnett()` function 
#' instead. Please be aware that that function is far slower when 
#' there are many comparison groups due to the nature of 
#' `mvtnorm::qmvt()` and high dimensional data.
#' 
#' @param data A data frame or tibble.
#' @param x A numeric vector that will be used to calculate the means.
#'   This can be a string or symbol.
#' @param treats A variable whose values are used to determine if the means
#'   are statistically significantly different from each other. Should be 
#'   a factor or character vector. This can be a string or symbol.
#' @param group <[`tidy-select`][dplyr_tidy_select]> A selection of columns to 
#'   group the data by in addition to `treats`. This operates very similarly
#'   to `.by` from dplyr (for more info on that see [?dplyr_by][dplyr_by]). 
#'   See examples to see how it operates.
#' @param wt Weights. Add if you have a weighting variable and want to perform
#'   Dunnett's test with weighted means.
#' @param ref_level A string that specifies the level of the reference group
#'   through which the others will be tested.
#' @param conf.level A number between 0 and 1 that signifies the width of the
#'   desired confidence interval. Default is 0.95, which corresponds to a 95%
#'   confidence interval.
#' @param decimals Number of decimals each number should be rounded to. Default is 2.
#' @param na.rm Logical. Determines if NAs should be removed
#' 
#' @return 
#' A `data.frame` with columns for the grouping variables, the 
#' treatment variable, the difference in means (`diffs`), 
#' number of observations (`n`), low CI (`conf.low`), high CI 
#' (`conf.high`), and p-value (`p_value`).
#' 
#' @examples
#' # load dply for the pipe: %>% 
#' library(dplyr)
#' library(adlgraphs)
#' 
#' # Check to see if any of the partisan groups are significantly different
#' # from the control group (in this case "Democrat") for conspiracy
#' # theory belief
#' get_diffs(test_data, "acts_avg", "pid_f3")
#'
#' # now do the same as above but make "Independent" the control group
#' get_diffs(test_data, "acts_avg", "pid_f3", ref_level = "Independent")
#'
#' # now let's add in education (`edu_f2`) as the `group` variable. This let's us
#' # compare education levels within each level of `edu_f2`. Note how the arguments
#' # don't have to be strings
#' get_diffs(test_data, acts_avg, pid_f3, edu_f2)
#' 
#' # we can also group by multiple variables. Due to a small n, I'm going to use 
#' # `edu_f2` instead of `edu_f`. 
#' test_data %>% 
#'   dplyr::mutate(values_f2 = make_dicho(values)) %>% 
#'   get_diffs(acts_avg, treats = pid_f3, group = c(edu_f2, values_f2))
#' 
#' # now let's do those previous two calculations but using `dplyr::group_by()`
#' test_data %>% 
#'   dplyr::group_by(pid_f3) %>% 
#'   get_diffs(acts_avg, edu_f)
#' 
#' # we can also group by multiple
#' test_data %>% 
#'   dplyr::mutate(values_f2 = make_dicho(values)) %>% 
#'   dplyr::group_by(pid_f3, values_f2) %>% 
#'   get_diffs(acts_avg, edu_f2)
#' 
#' @export
get_diffs <- function(
  data, 
  x, 
  treats, 
  group = NULL, 
  wt = NULL, 
  ref_level,
  decimals = 3, 
  conf.level = 0.95,
  na.rm = TRUE
) {

 

  # Ensure inputs are symbols or strings
  x_name <- deparse(substitute(x))
  x <- rlang::as_name(rlang::ensym(x))
  treats <- rlang::as_name(rlang::ensym(treats))
  
  ## Prepare group variables
  group_vars <- if (missing(group)) NULL else select_groups({{ group }}, data)
  group_vars <- group_vars[group_vars != "c"]
  group_names <- if (inherits(data, "grouped_df")) dplyr::group_vars(data) else NULL
  group_names <- unique(c(group_names, group_vars))
  # combine to the grouping variables and the treatments
  group_cols <- c(group_names, treats)
  
  # Check for numeric x and reference level presence
  if (!is.numeric(data[[x]])) {
    cli::cli_abort(c(
      "{.arg x} and must be of class `numeric`",
      "i" = "`{x_name}` is of class {class(data[[x]])}"
    ))
  }

  # Prepare weights
  if (missing(wt)) {
    wt <- "wts"
    data[[wt]] <- rep(1, nrow(data))
  } else {
    wt <- rlang::as_name(rlang::ensym(wt))
    data[[wt]][is.na(data[[wt]])] <- 0
  }

  # force the treats variable to a factor
  data[[treats]] <- make_factor(data[[treats]], drop_levels = TRUE, force = TRUE)

  # if rev_level is missing, set it to the first level in the treats variable
  if (missing(ref_level)) ref_level <- levels(data[[treats]])[1]
  

  data <- data[c(x, treats, group_names, wt)]
  if (na.rm) data <- data[stats::complete.cases(data), ]

  if (!is.null(group_names)) {

    # split up the data frame using vec_split 
    res <- vctrs::vec_split(
      # the data frame to split, use setdiff to get the columns not in group_names
      x = data[setdiff(colnames(data), group_names)],
      # split the data by the grouping variables
      by = data[group_names]
    )
    # create a nested data frame based on the split data from res
    nest_data <- vctrs::vec_cbind(
      # this creates columns with the levels from the variables used to split the data
      res$key, 
      # this creates a new tibble from each combination of levels used to split the data
      tibble::new_tibble(list(data = res$val))
    )
    # get the columns in group_names as a list and unname it
    cols <- unname(as.list(nest_data[group_names]))
    # using the list of columns, paste them together using rlang::exec and paste
    nest_data$name <- rlang::exec(paste, !!!cols, sep = "_")
    
    # iterate over each nest tibble from 
    out <- purrr::map(
      nest_data$data,
      ~bivariate_reg(
        .x,
        x = {{ x }}, 
        treats = {{ treats }},
        wt = {{ wt }}, 
        ref_level = ref_level, 
        conf.level = conf.level
      )
    ) %>% 
      stats::setNames(nest_data$name) 

    # combine the lists
    out <- vctrs::vec_rbind(!!!out, .names_to = "groups") %>% 
      tidyr::separate_wider_delim(cols = "groups", delim = "_", names = group_names)

  } else {
    out <- bivariate_reg(
      data = data,
      x = {{ x }}, 
      treats = {{ treats }},
      wt = {{ wt }}, 
      ref_level = ref_level, 
      conf.level = conf.level
    )
  }

  out[[treats]] <- gsub(pattern = treats, replacement = "", x = out$term, fixed = TRUE)
  
  out <- out[c(group_names, treats, "Estimate", "n", "conf.low", "conf.high", "Pr(>|t|)")]
  colnames(out) <- c(group_names, treats, "diffs", "n", "conf.low", "conf.high", "p_value")

  # set the grouping columns to factors
  out[group_cols] <- purrr::map(
    group_cols %>% setNames(nm = .),
    ~ make_factor(out[[.x]], levels = levels(data[[.x]]))
  )  
  out <- dplyr::arrange(out, dplyr::across(tidyselect::all_of(group_cols)))

  # round the numeric columns to decimals places
  out$diffs <- round(out$diffs, decimals)
  out$conf.low <- round(out$conf.low, decimals)
  out$conf.high <- round(out$conf.high, decimals)
  out$p_value <- round(out$p_value, decimals)


 
  # Add labels
  attr(out[[treats]], "label") <- attr(data[[treats]], "label")
  attr(out$diffs, "label") <- paste("Difference in means relative to", ref_level)
  attr(out$n, "label") <- "N"
  attr(out$conf.low, "label") <- "Low CI"
  attr(out$conf.high, "label") <- "High CI"
  attr(out$p_value, "label") <- "P-Value"
  attr(out, "variable_label") <- attr(data[[x]], "label")
  attr(out, "variable_name") <- x_name
  
  # Return results and set the class
  structure(out, class = c("adlgraphs_mean_diffs", class(out)))
}


bivariate_reg <- function(data, x, treats, wt, ref_level, conf.level = 0.95, decimals = 3) {
  
  data[[treats]] <- make_factor(data[[treats]])

  if (!missing(ref_level) && ref_level != levels(data[[treats]])[1]) {
    data[[treats]] <- stats::relevel(data[[treats]], ref_level)
  }
  
  # create the model
  model <- stats::lm(
    # use reformulate
    stats::reformulate(treats, x),
    data = data,
    weights = data[[wt]]
  )
  
  # calculate the number of observations per level
  n <- colSums(stats::model.matrix(model))[-1]

  # get the coefficients
  coefs <- summary(model)$coefficients %>%
    # make the rownames a column called "term"
    tibble::as_tibble(rownames = "term")

  # remove the intercept row
  out <- coefs[!grepl("(Intercept)", coefs$term, fixed = TRUE),] 
  # calculate z
  z <- stats::qt(1 - ((1 - conf.level) / 2), df = model$df.residual)
  # calculate the margin of error
  out$moe <- z * out$`Std. Error`
  out$conf.low <- out$Estimate - out$moe
  out$conf.high <- out$Estimate + out$moe
  out$n <- n
  return(out)

}




