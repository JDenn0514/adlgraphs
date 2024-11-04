#' Calculate means with confidence intervals
#'
#' Use this function to calculate simple weighted means with 95% confidence
#' intervals or weighted grouped means.
#'
#' The `x`, `group`, and `wt` arguments can either be strings or symbols
#' (meaning they can have quotes or no quotes). The benefit of this is that it
#' makes it really easy to iterate this function over a list or vector of
#' variables with other functions like [map()] [purrr::map()] or [walk()]
#' [purrr::walk()] that are found in the `purrr` package.
#'
#' @param data An object of type data.frame or tibble. If piping the data into the
#'   function, this is not required.
#' @param x Either a character string or symbol. The variable with which you want
#'   to get the mean.
#' @param group Either a character string or a symbol. The variable you want the
#'   means to be grouped by.
#' @param wt Weights. Add if you have a weighting variable and want to get
#'   weighted means
#' @param decimals Number of decimals to round the results to. Default is 2.
#'
#' @examples
#' # load the package
#' library(dplyr)
#'
#' # Let's calculate the overall average score for trad_n
#' get_means(test_data, trad_n)
#'
#' # it also works if x is a string
#' get_means(test_data, "trad_n")
#'
#' # Let's do that again but add weights
#' get_means(test_data, trad_n, wt = wts)
#'
#' # the wt argument can also be in quotes like this
#' get_means(test_data, "trad_n", wt = "wts")
#'
#' # Now let's do the average score for different education levels
#' get_means(test_data, trad_n, edu_f, wts)
#'
#' # it also works with quotes
#' get_means(test_data, "trad_n", "edu_f", "wts")
#'
#' # you can also pipe in the `data` argument if you want to do some data
#' # transformations before you calculate the means. For example, say you want
#' # to compare the means of `trad_n` among people who agreed vs disagreed with
#' # the variable `top`:
#' test_data %>%
#'   mutate(top_f2 = make_dicho(top)) %>%
#'   get_means(trad_n, top_f2, wts)
#'
#'
#'
#'
#' @export
#'
#'

get_means <- function(data, x, group, wt, decimals = 2) {

  # get the object's name
  x_lab <- deparse(substitute(x))

  # use enexpr() to capture the expressions supplied in "x"
  # enexpr returns a naked expression of the argument supplied in "x"
  # this is what allows the input to be either a string or a symbol
  x <- rlang::enexpr(x)

  if (!is.character(x)) {
    # if the name supplied in "x" is not a character...

    # capture x and convert to a symbol object with ensym()
    #then use as_name() to make it a string
    x <- rlang::as_name(rlang::ensym(x))

  }

  if (!is.numeric(data[[x]])) {
    # if the variable x is not numeric return an error

    cli::cli_abort(
      c(
        "`{x_lab}` must be a vector of class {.cls numeric}",
        x = "You've supplied a {.cls {class(x)}} vector"
      )
    )
  }


  if (!missing(group)) {
    # if group is not missing

    # use enexpr() to capture the expressions supplied in "group"
    # enexpr returns a naked expression of the argument supplied in "group"
    # this is what allows the input to be either a string or a symbol
    group <- rlang::enexpr(group)

    if (!is.character(group)) {
      # if the object supplied in "group" is not a character...

      # capture group and convert to a symbol object with ensym()
      #then use as_name() to make it a string
      group <- rlang::as_name(rlang::ensym(group))

    }

    if (is.numeric(data[[group]]) && !is.null(attr_val_labels(data[[group]]))) {
      # if group is class numeric AND DOES contain value labels

      # convert to a factor with sjlabelled
      data <- data %>%
        # convert to a factor using make_factor
        dplyr::mutate(group_f = make_factor(.data[[group]])) %>%
        # group by group_f
        dplyr::group_by(group_f)

    } else if (is.character(data[[group]]) || is.factor(data[[group]])) {
      # if group is of class character or factor return x

      data <- data %>%
        # just make a new variable called group_f comprised of gropu
        dplyr::mutate(group_f = .data[[group]]) %>%
        # group by group_f
        dplyr::group_by(group_f)


    } else {
      # if group is anything else (ie numeric)

      data <- data %>%
        # force to a factor
        dplyr::mutate(group_f = as.factor(.data[[group]])) %>%
        # group by group_f
        dplyr::group_by(group_f)

    }

  }

  if (missing(wt)) {
    # if missing wt

    # calculate the mean with CIs, standard deviation, standard error and N
    data_mean <- data %>%
      dplyr::summarise(
        # calculate the mean
        mean = mean(.data[[x]],  na.rm = TRUE),
        # calculate the standard deviation
        sd = sd(.data[[x]], na.rm = TRUE),
        # get the number of respondents
        n = dplyr::n()
      ) %>%
      dplyr::mutate(
        # calculate the standard error
        std.error = sd/sqrt(n),
        # calculate the lower CI
        conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
        # calculate the higher CI
        conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
        # round all of the numbers
        dplyr::across(dplyr::where(is.numeric), ~round(.x, decimals))
      ) %>%
      dplyr::select(-std.error)

    if (!missing(group)) {
      # if not missing group

      # rename the group_f variable with the name supplied in "group"
      data_mean <- data_mean %>% dplyr::rename({{ group }} := group_f)
      # give data_mean as the output
      return(data_mean)

    } else {
      # if group is missing

      # return the original data_mean as the output
      return(data_mean)

    }

  } else {
    # if wt is not missing

    # use enexpr() to capture the expressions supplied in "wt"
    # enexpr returns a naked expression of the argument supplied in "wt"
    # this is what allows the input to be either a string or a symbol
    wt <- rlang::enexpr(wt)

    if (!is.character(wt)) {
      # if the object supplied in "x" is not a character...

      # capture x and convert to a symbol object with ensym()
      #then use as_name() to make it a string
      wt <- rlang::as_name(rlang::ensym(wt))

    }


    data_mean <- data %>%
      dplyr::summarise(
        # calculate the mean
        mean = weighted.mean(.data[[x]], .data[[wt]], na.rm = TRUE),
        # calculate the standard deviation
        sd = sd(.data[[x]], na.rm = TRUE),
        # get the number of respondents
        n = dplyr::n()
      ) %>%
      dplyr::mutate(
        # calculate the standard error
        std.error = sd/sqrt(n),
        # calculate the lower CI
        conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
        # calculate the higher CI
        conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
        # round all of the numbers to the second decimal
        dplyr::across(dplyr::where(is.numeric), ~round(.x, decimals))
      ) %>%
      dplyr::select(-std.error)

    if (!missing(group)) {
      # if not missing group

      # rename the group_f variable with the name supplied in "group"
      data_mean <- data_mean %>% dplyr::rename({{ group }} := group_f)
      # give data_mean as the output
      return(data_mean)

    } else {
      # if group is missing

      # return the original data_mean as the output
      return(data_mean)

    }

  }

}



