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
#' Another important difference, is the addition of the `group` variable
#' and the ability to pipe in a grouped data frame. When `group` is specified or
#' a grouped data frame is piped in, the function still performs the Dunnett test 
#' between the variable in `x` and the variable in `treats`, but it does so 
#' along each level of whatever variable(s) are/is specified in `group` or that
#' the data is grouped by. You can see this in action in the examples section 
#' below.
#'
#' A third key difference (**WHICH HAS NOT BEEN IMPLEMENTED YET**) is the 
#' output. This function outputs a tibble object with a special class that 
#' enables it to be used with `prettytable()`. Furthermore, there are two 
#' distinct outputs: one with the means and one just the difference in means.
#'
#' Lastly, this function allows you to add weights to calculate weighted means.
#'
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
#'   Dunnett's test with weighted means,
#' @param control A string that specifies the level of the reference group
#'   through which the others will be tested.
#' @param conf.level A number between 0 and 1 that signifies the width of the
#'   desired confidence interval. Default is 0.95, which corresponds to a 95%
#'   confidence interval.
#' @param show_means Logical. Determines if the output should contain the means
#'   of each level. Default is `FALSE`
#' @param show_diffs Logical. Determines if the output should contain the 
#'   difference in means
#' @param na.rm Logical. Determines if NAs should be removed
#'
#' @examples
#' 
#' # load dply for the pipe: %>% 
#' library(dplyr)
#' library(adlgraphs)
#' 
#' # Check to see if any of the education groups are significantly different
#' # from the control group (in this case "High School or Less") for conspiracy
#' # theory belief
#' dunnett(test_data, "acts_avg", "pid_f3")
#' # now let's do the same but have it show the means
#' dunnett(test_data, "acts_avg", "pid_f3", show_means = TRUE)
#'
#' # now do the same as above but make "Graduate Degree" the control group
#' dunnett_helper(test_data, "acts_avg", "pid_f3", control = "Independent")
#' dunnett(test_data, "acts_avg", "pid_f3", control = "Independent", show_means = TRUE)
#'
#' # now let's add in partisanship (`edu_f2`) as the `group` variable. This let's us
#' # compare education levels within each level of `edu_f2`. Note how the arguments
#' # don't have to be strings
#' dunnett(test_data, acts_avg, pid_f3, edu_f2)
#' 
#' # we can also group by multiple variables. Due to a small n, I'm going to use 
#' # `edu_f2` instead of `edu_f`. 
#' test_data %>% 
#'   dplyr::mutate(values_f2 = make_dicho(values)) %>% 
#'   dunnett(acts_avg, treats = pid_f3, group = c(edu_f2, values_f2))
#' 
#' # now let's do those previous two calculations but using `dplyr::group_by()`
#' test_data %>% 
#'   dplyr::group_by(pid_f3) %>% 
#'   dunnett(acts_avg, edu_f)
#' 
#' # we can also group by multiple
#' test_data %>% 
#'   dplyr::mutate(values_f2 = make_dicho(values)) %>% 
#'   dplyr::group_by(pid_f3, values_f2) %>% 
#'   dunnett(acts_avg, edu_f2)
#' 
#' # If we want to show means and differences, set show_means to TRUE
#' # we don't need to set show_diffs to TRUE since that is the default
#' dunnett(test_data, acts_avg, edu_f, show_means = TRUE)
#' 
#' # if we want to show means without differences, set `show_diffs = FALSE`
#' dunnett(test_data, acts_avg, edu_f, show_means = TRUE, show_diffs = FALSE)
#' 
#'
#' @export
#' 

dunnett <- function(
  data,
  x,
  treats = NULL,
  group = NULL,
  wt = NULL,
  control = NULL,
  conf.level = 0.95,
  show_means = FALSE,
  show_diffs = TRUE,
  na.rm = TRUE
) {
  UseMethod("dunnett")
}

#' @export
dunnett.data.frame <- function(
  data,
  x,
  treats = NULL,
  group = NULL,
  wt = NULL,
  control = NULL,
  conf.level = 0.95,
  show_means = FALSE,
  show_diffs = TRUE,
  na.rm = TRUE
) {

  treats <- accept_string_or_sym({{ treats }})
  x <- accept_string_or_sym({{ x }})

  # get the variable name for treats but remove the quotation mark
  if (is.character(treats)) {
    treats_name <- rlang::sym(treats)
  } else {
    treats_name <- rlang::enexpr(treats)
  }

  if (is.character(x)) {
    x_name <- rlang::sym(x)
  } else {
    x_name <- rlang::enexpr(x)
  }

  group <- rlang::enquo(group)

  if (!rlang::quo_is_null(group)) {

    group_expr <- rlang::enexpr(group)
    # create a vector of the grouping variables
    group_enquo <- eval_select_by(group, data)

    # group the data
    data <- data %>% dplyr::group_by(dplyr::across(tidyselect::all_of(group_enquo))) %>% 
      # remove unnecessary columns
      dplyr::select(tidyselect::all_of(group_enquo), {{ x }}, {{ treats }})
 
    if (isTRUE(na.rm)) {
      data <- na.omit(data)
    }

    # get the group helpers (nest_data, group_labs, and just_groups)
    group_helpers <- group_analysis_helper(data = data)
  
    # make the correlation dataframe
    dunn_df <- furrr::future_map(
      # we are iterating over the data column
      group_helpers$nest_data$data, 
      # use get_all_corr.data.frame to get the individuals loadings
      ~dunnett_helper(
        .x,
        x = {{ x }},
        treats = {{ treats }},
        wt = {{ wt }},
        control = {{ control }},
        conf.level = {{ conf.level }},
        show_means = show_means,
        show_diffs = show_diffs
      ),
      .options = furrr::furrr_options(seed = NULL)
    ) 
    # name the objects in the list
    names(dunn_df) <- group_helpers$just_groups
  
    if (length(group_helpers$nest_data) > 2) {
      # if there are two or more grouping variables do the following
    
      # create the output data frame
      # combine the list objects and make a new variable 
      # called "groups" containing the names of each list
      out <- data.table::rbindlist(dunn_df, idcol = "groups") %>% 
        # convert to a tibble
        tibble::as_tibble() %>% 
        # split up the string by the - and use the group_labs vector as the names
        tidyr::separate_wider_delim(groups, delim = " - ", names = c(group_helpers$group_labs)) %>% 
        # group the data by the vector group variables
        dplyr::group_by(dplyr::across(tidyselect::all_of(group_helpers$group_labs))) 

      # make the grouping variables factors
      out[group_helpers$group_labs] <- lapply(
        group_helpers$group_labs, 
        function(lab) {
          factor(out[[lab]], levels = levels(group_helpers$nest_data[[lab]]))
        }
      )
  
      # arrange by the groups
      out <- out %>% dplyr::arrange(.by_group = TRUE) 

    } else {
      # create the output data frame
      # combine the list objects and make a new variable 
      # called "groups" containing the names of each list
      out <- data.table::rbindlist(dunn_df, idcol = group_helpers$group_labs) %>%
        # force it to a tibble
        tibble::as_tibble()

      # set the factor levels
      out[[group_helpers$group_labs]] <- factor(
        out[[group_helpers$group_labs]],
        levels = levels(group_helpers$nest_data[[group_helpers$group_labs]])
      )

      # arrange by the groups
      out <- out %>% dplyr::arrange(.by_group = TRUE) 
    }
  
  } else {

    out <- dunnett_helper(
      data = data,
      x = {{ x }},
      treats = {{ treats }},
      wt = {{ wt }},
      control = {{ control }},
      conf.level = {{ conf.level }},
      show_means = show_means,
      show_diffs = show_diffs
    )
    return(out)

  }

  if ("mean" %in% colnames(out)) {

    attr(out$mean, "label") <- "Mean"
    attr(out$sd, "label") <- "SD"

    if (!is.null(levels(data[[treats]]))) {
      # get teh treatment levels
      treat_levels <- levels(data[[treats]])
      # set the treats variable as a factor and set the levels
      out[[treats]] <- factor(
        out[[treats]],
        levels = treat_levels
      )
    }

  } 

  if ("diff" %in% colnames(out)) {

    attr(out$diff, "label") <- paste("Difference relative to", control)

    if (isFALSE(show_means) && !is.null(levels(data[[treats]]))) {
      
      if (!is.null(control)) {
        data[[treats]] <- forcats::fct_relevel(data[[treats]], control)
      }
      # get teh treatment levels
      treat_levels <- levels(data[[treats]])[-1]
      # set the treats variable as a factor and set the levels
      out[[treats]] <- factor(
        out[[treats]],
        levels = treat_levels
      )
       
    }

  }

  # set the variable labels
  attr(out[[treats]], "label") <- attr_var_label(data[[treats]])
  attr(out$n, "label") <- "N"
  attr(out$conf.low, "label") <- "Low CI"
  attr(out$conf.high, "label") <- "High CI"
  attr(out$p.value, "label") <- "P-Value"

  attr(out, "variable_label") <- attr_var_label(data[[x]])
  attr(out, "variable_name") <- x_name

  ### add a special class to this df so that it can have a unique 
  ### prettytable appearance and function
  # get the class names
  class_names <- class(out)
  # add a special class to this function
  out %>% structure(class = c("adlgraphs_dunnett", class_names))

  

}

#' @export
dunnett.grouped_df <- function(
  data,
  x,
  treats = NULL,
  group = NULL,
  wt = NULL,
  control = NULL,
  conf.level = 0.95,
  show_means = FALSE,
  show_diffs = TRUE,
  na.rm = TRUE
) {

  treats <- accept_string_or_sym({{ treats }})
  x <- accept_string_or_sym({{ x }})

  # get the variable name for treats but remove the quotation mark
  if (is.character(treats)) {
    treats_name <- rlang::sym(treats)
  } else {
    treats_name <- rlang::enexpr(treats)
  }

  if (is.character(x)) {
    x_name <- rlang::sym(x)
  } else {
    x_name <- rlang::enexpr(x)
  }

  # keep only relevant variables
  data <- dplyr::select(data, tidyselect::all_of(dplyr::group_vars(data)), {{ x }}, {{ treats }})

  if (isTRUE(na.rm)) {
    data <- na.omit(data)
  }

  group_helpers <- group_analysis_helper(data = data)
  
  # make the correlation dataframe
  dunn_df <- furrr::future_map(
    # we are iterating over the data column
    group_helpers$nest_data$data, 
    # use get_all_corr.data.frame to get the individuals loadings
    ~dunnett_helper(
      .x,
      x = {{ x }},
      treats = {{ treats }},
      wt = wt,
      control = control,
      conf.level = conf.level,
      show_means = show_means,
      show_diffs = show_diffs
    ),
    .options = furrr::furrr_options(seed = NULL)
  ) 
  # name the objects in the list
  names(dunn_df) <- group_helpers$just_groups

  if (length(group_helpers$nest_data) > 2) {
    # if there are two or more grouping variables do the following
  
    # create the output data frame
    # combine the list objects and make a new variable 
    # called "groups" containing the names of each list
    out <- data.table::rbindlist(dunn_df, idcol = "groups") %>% 
      # force as a tibble
      tibble::as_tibble() %>% 
      # split up the string by the - and use the group_labs vector as the names
      tidyr::separate_wider_delim(groups, delim = " - ", names = c(group_helpers$group_labs)) %>% 
      # group the data by the vector group variables
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_helpers$group_labs)))

      # make the grouping variables factors
      out[group_helpers$group_labs] <- lapply(
        group_helpers$group_labs, 
        function(lab) {
          factor(out[[lab]], levels = levels(group_helpers$nest_data[[lab]]))
        }
      )
  
      # arrange by the groups
      out <- out %>% dplyr::arrange(.by_group = TRUE) 
    
  } else {
  
    # create the output data frame
    # combine the list objects and make a new variable 
    # called "groups" containing the names of each list
    out <- data.table::rbindlist(dunn_df, idcol = group_helpers$group_labs) %>% 
      # force as tibble
      tibble::as_tibble() 

      # set the factor levels
      out[[group_helpers$group_labs]] <- factor(
        out[[group_helpers$group_labs]],
        levels = levels(group_helpers$nest_data[[group_helpers$group_labs]])
      )

      # arrange by the groups
      out <- out %>% dplyr::arrange(.by_group = TRUE) 
  
  }
    

  if ("mean" %in% colnames(out)) {

    attr(out$mean, "label") <- "Mean"
    attr(out$sd, "label") <- "SD"

  } 

  if ("diff" %in% colnames(out)) {

    attr(out$diff, "label") <- paste("Difference relative to", control)

  }

  # set the variable labels
  attr(out[[treats]], "label") <- attr_var_label(data[[treats]])
  attr(out$n, "label") <- "N"
  attr(out$conf.low, "label") <- "Low CI"
  attr(out$conf.high, "label") <- "High CI"
  attr(out$p.value, "label") <- "P-Value"

  attr(out, "variable_label") <- attr_var_label(data[[x]])
  attr(out, "variable_name") <- x_name

  ### add a special class to this df so that it can have a unique 
  ### prettytable appearance and function
  # get the class names
  class_names <- class(out)
  # add a special class to this function
  out %>% structure(class = c("adlgraphs_dunnett", class_names))

}






#' Perform Dunnett's test (mostly internal function)
#'
#' This function was created to mostly serve as an internal function for
#' `dunnett()`. It does all the same things as `dunnett()`, however,
#' you can't group the data and then do the calculation.
#'
#' @inheritParams dunnett
#'
#' @export
dunnett_helper <- function(
    data,
    x,
    treats,
    wt = NULL,
    control = NULL,
    conf.level = 0.95,
    show_means = FALSE,
    show_diffs = TRUE
) {

  if (isFALSE(show_means) && isFALSE(show_diffs)) {
    cli::cli_abort(c(
      x = "{.arg show_means} and {.arg show_diffs} are both set to `FALSE`",
      i = "At least one must be set to `TRUE`"
    ))
  }

  # ensure that strings or symbols for x work
  x <- accept_string_or_sym({{ x }})
  # ensure that strings or symbols for treats work
  treats <- accept_string_or_sym({{ treats }})
  if (!is.null(control)) {
    control <- accept_string_or_sym({{ control }})
  }
  

  # get the variable name for treats but remove the quotation mark
  if (is.character(treats)) {
    treats_name <- rlang::sym(treats)
  } else {
    treats_name <- rlang::enexpr(treats)
  }

  # extract the vector for x
  x_vec <- data[[x]]
  # extract the vector of treats
  treats_vec <- data[[treats]]

  # if treats_vec is not a factor make it one
  if (!is.factor(treats_vec)) {
    treats_vec <- make_factor(treats_vec)
  }
  # get the unique observations in treats_vec
  obs <- unique(treats_vec)
  # get the levels in treats_vec
  lvls <- levels(treats_vec)

  # keep only the levels that are also observations
  new_lvls <- intersect(lvls, obs)

  treats_vec <- factor(treats_vec, levels = new_lvls)

  # get the number of levels in treats_vec
  k <- nlevels(treats_vec)
  # get the number of observations in x_vec
  N <- length(x_vec)

  if (is.null(control)) control <- levels(treats_vec)[1]

  #### do the analysis ------------------------------------------------
  out <- list()

  # calculate length of x_vec across each level in treats_vec
  ni <- tapply(x_vec, treats_vec, length)

  if (!is.null(wt)) {
    # use split to split the data
    means <- sapply(split(data, treats_vec), function(z) weighted.mean(z[[x]], z[[wt]]))
  } else {
    # calculate mean of x_vec across each level in treats_vec
    means <- tapply(x_vec, treats_vec, mean)
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
  s <- sqrt( sum(tapply(x_vec, treats_vec, function(x) sum((x - mean(x))^2) )) / (N - k))

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


  p.value <- sapply(seq_len(k - 1), function(i) {
    1 - mvtnorm::pmvt(
      lower = -abs(Dj[i]),
      upper = abs(Dj[i]),
      corr = R,
      delta=rep(0, k-1),
      df = N - k
    )[1]
  })
  

  #### create the final output -------------------------------------------------

  # create the output table of differences
  diffs <- tibble::tibble(
    # add the treatment variable
    "{ treats_name }" := names(meandiffs),
    # add the difference in means and coerce to vector
    diff = as.vector(meandiffs),
    # add the number of observations and coerce to vector
    n = as.vector(fittedn),
    # add low CI and coerce to vector
    conf.low = as.vector(conf.low),
    # add high CI
    conf.high = as.vector(conf.high),
    # add the p.value
    p.value,
    # add the stars based on p-value
    stars = stars_pval(p.value)
  ) %>%
    # round all numeric variables to the third decimal
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        ~round(.x, 3)
      )
    )

  if (isTRUE(show_means)) {
    ### calculate the means if show_means is TRUE

    # calculate the means
    means <- data %>%
      # group the data by treats
      dplyr::group_by(.data[[treats]]) %>%
      # calculate the means
      get_means({{ x }})

    control_df <- data.frame(
      # the value in the treatments column is set to whatever the control is
      treats = control,
      # set difference to zero
      diff = 0,
      # add the number observations in control and coerce to a vector
      n = as.vector(controln),
      # set low CI to NA
      conf.low = NA,
      # set high CI to NA
      conf.high = NA,
      # set p.value to NA
      p.value = NA,
      # set stars to NA
      stars = NA
    ) %>% 
    # rename the treatments column 
      dplyr::rename("{ treats_name }" := treats)
    
    diffs <- rbind(control_df, diffs)
    

    if (isTRUE(show_diffs)) {
      ### do the calculations if we want to see the differences and means

      # keep only the relevant variables: treatments, differencs, p.value and stars
      diffs <- diffs %>% dplyr::select(diff, p.value, stars) 
      
      # join the diffs df with the means dfs 
      cbind(means, diffs) %>% 
        # move the diff column so that it appears after mean
        dplyr::relocate(diff, .after = mean) %>% 
        dplyr::as_tibble()
      
    } else {
      ### if we don't want to see the differences then just do the folllowing:

      # keep only the relevant variables: treatments, p.value, and stars
      # we don't want the diffs so remove them
      diffs <- diffs %>% dplyr::select( p.value, stars) 
      # join the means and diffs dfs together for the final df
      dplyr::as_tibble(cbind(means, diffs))
    }

  } else {
    # if we don't want the means then 
    dplyr::as_tibble(diffs)

  }

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
