#' Run Dunnett's multiple comparisons test with one control.
#'
#' `dunnett()` calculates Dunnett's post hoc pairwise multiple comparisons
#' procedure. More simply, it calculates the mean of a variable (`x`) along the
#' different levels of a grouping variable (`treats`) and then determines if the
#' difference between the control/reference group and each level is statistically
#' significant.
#'
#' While there are other functions that also perform Dunnett's Test, like
#' [PMCMRplus::dunnettTest()] and [DescTools::DunnettTest()] to name a few,
#' there are a few key differences between this function and those. Firstly,
#' this function takes in a data frame or tibble. This was done so that it can
#' either be piped in or specified in the argument. See more in the example
#' section below.
#'
#' Another important difference, is the addition of the `group` variable.
#' When `group` is specified, the function still performs the Dunnett test between
#' the variable in `x` and the variable in `treats`, but it does so along each
#' level of whatever variable is specified in `group`. You can see this in
#' action in the examples section below.
#'
#' A third key difference is the output. This function outputs a tibble object
#' with a special class that enables it to be used with `prettytable()`.
#' Furthermore, there are two distinct outputs. The default output shows the
#' difference in means between each level and the control level and the
#' confidence intervals are based on this difference. However, if
#' `show_means = TRUE`, then the output shows the mean values for each level in
#' `treats` while still indicating the p-value.
#'
#' Lastly, this function allows you to add weights to calculate weighted means.
#'
#'
#' @param data A data frame or tibble.
#' @param x A numeric vector that will be used to calculate the means.
#' @param treats A variable whose values are used to determine if the means
#'   are statistically significantly different from each other.
#' @param group A character or factor vector. It is a second level grouping
#'   variable. If specified, the function checks to see if the values in
#'   `treats` are statistically significant  within each level of this variable.
#' @param wt Weights. Add if you have a weighting variable and want to perform
#'   Dunnett's test with weighted means,
#' @param control A string that specifies the level of the reference group
#'   through which the others will be tested.
#' @param conf.level A number between 0 and 1 that signifies the width of the
#'   desired confidence interval. Default is 0.95, which corresponds to a 95%
#'   confidence interval.
#' @param show_means Logical. Determines if the output should contain the mean
#'   differences between the levels and the reference level or if it should show
#'   the means of each level. Default is `FALSE`
#'
#' @examples
#' # Check to see if any of the education groups are significantly different
#' # from the control group (in this case "High School or Less") for conspiracy
#' # theory belief
#' dunnett(test_data, "acts_avg", "edu_f")
#' # now let's do the same but have it show the means
#' dunnett(test_data, "acts_avg", "edu_f", show_means = TRUE)
#'
#' # now do the same as above but make "Graduate Degree" the control group
#' dunnett(test_data, "acts_avg", "edu_f", control = "Graduate Degree")
#' dunnett(test_data, "acts_avg", "edu_f", control = "Graduate Degree", show_means = TRUE)
#'
#' # now let's add in partisanship (`pid_f3`) as the `group` variable. This let's us
#' # compare education levels within each level of `pid_f3`.
#' dunnett(test_data, "acts_avg", "edu_f", "pid_f3")
#' # and repeat that but showing the means
#' dunnett(test_data, "acts_avg", "edu_f", "pid_f3", show_means = TRUE)
#'
#' @export

dunnett <- function(
  data,
  x,
  treats = NULL,
  group = NULL,
  wt = NULL,
  control = NULL,
  conf.level = 0.95,
  show_means = FALSE
) {

  ## get the variable names for treats and group but remove the quotation mark
  treats_name <- deparse(substitute(treats)) %>%
    gsub('[\"]', "", .)

  if (!is.null(group)) {
    # if group is not null, create a list object where the data is split by the
    # levels of the group
    df <- split(data, data[[group]])

    # set the names of the df with the levels in group
    # df <- stats::setNames(df, forcats::fct_unique(make_factor(data[[group]])))

    # get the group variable name
    group_name <- rlang::enexpr(group) %>% 
      gsub('[\"]', "", .)
    

    # get the number of groups in the data
    leng <- length(df)

    # use lapply to calculate the pvalues across the data frames in the df list
    out <- lapply(1:leng, function(n) dunnett_pval_fun(df, x = x, group = group, treats = treats, wt = wt, n = n, control = control, conf.level = conf.level, show_means = show_means)) %>%
      # combine the data.frames in the list
      dplyr::bind_rows() %>%
      # rename the variables
      dplyr::rename(
        {{ group_name}} := group,
        {{ treats_name }} := treats
      )

    
    if (isTRUE(show_means)) {
      # now we need to calculate the means
      mean_table <- data %>%
        # drop NAs, use all_of() for data-masking (changed in tidy-select 1.2)
        tidyr::drop_na(tidyselect::all_of(treats), tidyselect::all_of(group)) %>%
        # group the data by group and treats
        dplyr::group_by(.data[[group]], .data[[treats]]) %>%
        # calculate the means
        get_means({{ x }}) %>% 
        dplyr::ungroup()

      
      # only keep the group, the treats, and pval
      diffs <- out %>% 
        dplyr::ungroup() %>% 
        dplyr::select(tidyselect::all_of(treats), tidyselect::all_of(group), p.value, stars)

      # join the means with the pvals from the differences
      out <- dplyr::full_join(mean_table, diffs)

      # get the class names
      class_names <- class(out)

      # add a special class to this function
      out <- out %>% structure(class = c("adlgraphs_dunnett_means", class_names))

    } else {

      # get the class names
      class_names <- class(out)

      # add a special class to this function
      out <- out %>% structure(class = c("adlgraphs_dunnett_diffs", class_names))

    }

    if (!is.null(out[[group]])) {
      attr(out[[group]], "label") <- attr_var_label(data[[group]])
    }

  } else {
    # calculat the pvalues
    out <- dunnett_pval_fun(data, x = x, treats = treats, control = control, wt = wt, conf.level = conf.level, show_means = show_means)  %>%
      dplyr::rename({{ treats_name }} := treats)

    if (isTRUE(show_means)) {
      # now we need to calculate the means
      mean_table <- data %>%
        tidyr::drop_na(tidyselect::all_of(treats)) %>%
        dplyr::group_by(.data[[treats]]) %>%
        get_means({{ x }}) %>% 
        dplyr::ungroup()

      # only keep the group, the treats, and pval
      diffs <- out %>% 
        dplyr::ungroup() %>% 
        dplyr::select(tidyselect::all_of(treats), p.value, stars)

      # join the means with the pvals from the differences
      out <- dplyr::full_join(mean_table, diffs)

      # get the class names
      class_names <- class(out)

      # add a special class to this function
      out <- out %>% structure(class = c("adlgraphs_dunnett_means", class_names))

    } else {
      # get the class names
      class_names <- class(out)

      # add a special class to this function
      out <- out %>% structure(class = c("adlgraphs_dunnett_diffs", class_names))

    }

  }
  

  control <- levels(make_factor(data[[treats]]))[1]

  if ("mean" %in% colnames(out)) {
    attr(out$mean, "label") <- "Mean"
    attr(out$sd, "label") <- "SD"
  } else {
    attr(out$diff, "label") <- paste("Difference in means relative to", control)
  }

  # set the variable labels
  attr(out[[treats]], "label") <- attr_var_label(data[[treats]])
  attr(out$n, "label") <- "N"
  attr(out$conf.low, "label") <- "Low CI"
  attr(out$conf.high, "label") <- "High CI"
  attr(out$p.value, "label") <- "P-Value"

  return(out)

}





#' Perform Dunnett's test (mostly internal function)
#'
#' This function was created to mostly serve as an internal function for
#' `Dunnett`. It is the main working function that calculates the p-values.
#'
#' @inheritParams dunnett
#' @param n If using a list of dataframes, this specifies which data.frame in
#'   the list is being used to perform Dunnett's test.
#'
#' @export
dunnett_pval_fun <- function(
    data,
    x,
    treats,
    group = NULL,
    wt = NULL,
    n,
    control = NULL,
    conf.level = 0.95,
    show_means = FALSE
) {


  ## get the variable names for treats and group but remove the quotation mark
  treats_name <- deparse(substitute(treats)) %>%
    gsub('[\"]', "", .)

  if (!is.null(group)) {
    yar <- TRUE

    group_name <- deparse(substitute(group)) %>%
      gsub('[\"]', "", .)

    #### prep the data for the analysis ------------------------------------------
    # get the data set for the specific group
    data <- data[[n]]

    # get the specific group that is in the data set
    var <- unique(data[[group]])


  }


  # drop NAs, use all_of() for data-masking (changed in tidy-select 1.2)
  data <- data %>% tidyr::drop_na(tidyselect::all_of(x), tidyselect::all_of(treats))

  # extract the vector for x
  x_vec <- data[[x]]

  # extract the vector of treats
  treats <- data[[treats]]

  # if treats is not a factor make it one
  if (!is.factor(treats)) {
    treats <- make_factor(treats)
  }
  # get the unique observations in treats
  obs <- unique(treats)
  # get the levels in treats
  lvls <- levels(treats)
  # keep only the levels that are also observations
  new_lvls <- intersect(lvls, obs)

  treats <- factor(
    treats,
    levels = new_lvls
  )

  # get the number of levels in treats
  k <- nlevels(treats)
  # get the number of observations in x_vec
  N <- length(x_vec)

  if (is.null(control)) control <- levels(treats)[1]

  #### do the analysis ------------------------------------------------
  out <- list()

  # calculate length of x_vec across each level in treats
  ni <- tapply(x_vec, treats, length)

  if (!is.null(wt)) {
    # use split to split the data
    means <- sapply(split(data, treats), function(z) weighted.mean(z[[x]], z[[wt]]))
  } else {
    # calculate mean of x_vec across each level in treats
    means <- tapply(x_vec, treats, mean)
  }
  # get the difference in the means between treatments and control group
  meandiffs <- means[names(means) != control] - means[control]

  # get the number of observations for each treatment, excluding the control
  fittedn <- ni[names(ni) != control]
  # get the number of observations for just the control group
  controln <- ni[control]

  #calculate the pooled sd across groups defined by g
  # numerator = Sum of squared deviations from the mean within each group (summed across all groups)
  # denominator = Degrees of freedom for the pooled estimate (total number of observations minus the number of groups)
  # Square root: To convert the variance estimate to a standard deviation.
  s <- sqrt( sum(tapply(x_vec, treats, function(x) sum((x - mean(x))^2) )) /
               (N - k))

  # calculate the t-statistic for the difference in means between the treatment groups and the reference group
  # numerator = difference in means
  # denominator = standard error of the difference
  Dj <- meandiffs / (s * sqrt((1/fittedn) + (1/controln)))
  # calculates the square root of the proportion of the total sample size that is in the treatment group
  Rij <- sqrt(fittedn/(fittedn + controln))

  #line creates a matrix R where each element is the product of the corresponding elements in Rij
  # The outer function computes the outer product of Rij with itself using multiplication (*)
  R <- outer(Rij, Rij, "*")
  # Set the diagonal elements of the matrix to 1. this makes the matrix a
  # correlation matrix with ones on the diagonal and the products of Rij off the
  # diagonal. This ensures that each group's weight relative to itself is 1.
  diag(R) <- 1
  # Michael Chirico suggests in https://github.com/AndriSignorell/DescTools/pull/102
  withr::with_seed(5, {
    # calculate the quantile of the multivariate t-distribution, which will be
    # used to calculate the confidence intervals.
    qvt <- mvtnorm::qmvt((1 - (1 - conf.level)/2), df = N - k, sigma = R, tail = "lower.tail")$quantile
  })

  conf.low <- meandiffs - s * sqrt((1/fittedn) + (1/controln)) * qvt
  conf.high <- meandiffs + s * sqrt((1/fittedn) + (1/controln)) * qvt


  p.value <- c()
  for (i in 1:(k - 1)){
    p.value[i] <- 1 - mvtnorm::pmvt(
      lower = -abs(Dj[i]),
      upper = abs(Dj[i]),
      corr = R,
      delta=rep(0, k-1),
      df = N - k
    )[1]
  }

  #### create the final output -------------------------------------------------

  # create the output table
  out <- tibble::tibble(
    # add the treatment variable
    "{ treats_name }" := names(meandiffs),
    # add the difference in means
    diff = as.vector(meandiffs),
    n = as.vector(fittedn),
    conf.low = as.vector(conf.low),
    conf.high = as.vector(conf.high),
    p.value,
    stars = stars_pval(p.value)
  ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        ~round(.x, 3)
      )
    )

  if (isTRUE(show_means)) {

    out <- out %>%
      tibble::add_row(
        "{ treats_name }" := control,
        diff = 0,
        n = as.vector(controln),
        conf.low = NA,
        conf.high = NA,
        p.value = NA,
        stars = NA
      )

  }

  if (!is.null(group)) {
    #return(out)
    out <- out %>%
      tibble::add_column("{ group_name }" := var, .before = 1)
  }

  return(out)

}


#' Add stars based on the p-value
#' @param p.value A vector of p-values to determine what stars to give
#' @export
stars_pval <- function(p.value) {
  unclass(
    stats::symnum(
      p.value,
      corr = FALSE, na = FALSE,
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c("***", "**", "*", ".", "")
    )
  )
}
