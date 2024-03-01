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
#' @param df An object of type data.frame or tibble. If piping the df into the
#'   function, this is not required.
#' @param x Either a character string or symbol. The variable with which you want
#'   to get the mean.
#' @param group Either a character string or a symbol. The variable you want the
#'   means to be grouped by.
#' @param wt Weights. Add if you have a weighting variable and want to get
#'   weighted means
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
#' # you can also pipe in the `df` argument if you want to do some data
#' # transformations before you calculate the means. For example, say you want
#' # to compare the means of `trad_n` among people who agreed vs disagreed with
#' # the variable `prod_isr`:
#' test_data %>%
#'   mutate(prod_isr_f2 = make_dicho(prod_isr)) %>%
#'   get_means(trad_n, prod_isr_f2, wts)
#'
#'
#'
#'
#' @export
#'
#'

get_means <- function(df, x, group, wt) {

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

  if (!is.numeric(df[[x]])) {
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

    if (haven::is.labelled(df[[group]])) {
      # if group is haven_labelled

      df <- df %>%
        # convert to a factor using haven::as_factor
        dplyr::mutate(group_f = haven::as_factor(.data[[group]])) %>%
        # group by group_f
        dplyr::group_by(group_f)


    } else if (is.numeric(df[[group]]) && !is.null(sjlabelled::get_labels(df[[group]]))) {
      # if group is class numeric AND DOES contain value labels

      # convert to a factor with sjlabelled
      df <- df %>%
        # convert to a factor using haven::as_factor
        dplyr::mutate(group_f = sjlabelled::as_label(.data[[group]])) %>%
        # group by group_f
        dplyr::group_by(group_f)

    } else if (is.character(x) || is.factor(x)) {
      # if group is of class character or factor return x

      df <- df %>%
        # just make a new variable called group_f comprised of gropu
        dplyr::mutate(group_f = .data[[group]]) %>%
        # group by group_f
        dplyr::group_by(group_f)

    } else {
      # if group is anything else (ie numeric)

      df <- df %>%
        # force to a factor
        dplyr::mutate(group_f = as.factor(group)) %>%
        # group by group_f
        dplyr::group_by(group_f)

    }

  }

  if (missing(wt)) {
    # if missing wt

    # calculate the mean with CIs, standard deviation, standard error and N
    df_mean <- df %>%
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
        # round all of the numbers to the second decimal
        dplyr::across(dplyr::where(is.numeric), ~round(.x, 2))
      )

    if (!missing(group)) {
      # if not missing group

      # rename the group_f variable with the name supplied in "group"
      df_mean <- df_mean %>% dplyr::rename({{ group }} := group_f)
      # give df_mean as the output
      return(df_mean)

    } else {
      # if group is missing

      # return the original df_mean as the output
      return(df_mean)

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


    df_mean <- df %>%
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
        dplyr::across(dplyr::where(is.numeric), ~round(.x, 2))
      )

    if (!missing(group)) {
      # if not missing group

      # rename the group_f variable with the name supplied in "group"
      df_mean <- df_mean %>% dplyr::rename({{ group }} := group_f)
      # give df_mean as the output
      return(df_mean)

    } else {
      # if group is missing

      # return the original df_mean as the output
      return(df_mean)

    }

  }

}
