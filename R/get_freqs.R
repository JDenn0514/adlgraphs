#' Calculate weighted frequencies
#'
#' Use this function to calculate simple weighted frequencies weighted grouped.
#' You can also specify a grouping variable by which you want to calculate the
#' frequencies
#'
#' The `x`, `group`, and `wt` arguments can either be strings or symbols
#' (meaning they can have quotes or no quotes). The benefit of this is that it
#' makes it really easy to iterate this function over a list or vector of
#' variables with other functions like [map()] [purrr::map()] or [walk()]
#' [purrr::walk()] that are found in the `purrr` package.
#'
#' @param data An object of type data.frame or tibble. If piping the data into the
#'   function, this is not required.
#' @param x Either a character string or symbol. The variable with which want
#'   to get the frequencies.
#' @param group Either a character string or a symbol. The grouping variable.
#' @param wt Weights. Add if you have a weighting variable and want to get
#'   weighted frequencies
#' @param cross_tab Logical. If a `group` object has been supplied, should the
#'   the table be pivoted to create make it like crosstabs
#'
#' @examples
#' # load the package
#' library(dplyr)
#'
#' # Let's calculate the overall frequency for big_events
#' get_freqs(test_data, big_events)
#'
#' # it also works if x is a string
#' get_means(test_data, "big_events")
#'
#' # Let's do that again but add weights
#' get_freqs(test_data, big_events, wt = wts)
#'
#' # the wt argument can also be in quotes like this
#' get_freqs(test_data, "big_events", wt = "wts")
#'
#' # Now let's do the average score for different education levels
#' get_freqs(test_data, big_events, edu_f, wts)
#'
#' # it also works with quotes
#' get_freqs(test_data, "big_events", "edu_f", "wts")
#'
#' # if we want to pivot the results so they look like cross tabs, then we need
#' # to set `cross_tab` to TRUE
#' get_freqs(test_data, big_events, edu_f, wts, cross_tab = TRUE)

#'
#' # you can also pipe in the `data` argument if you want to do some data
#' # transformations before you calculate the means. For example, say you want
#' # to compare the frequencies of `big_events` among people who agreed vs
#' # disagreed with the variable `top`:
#' test_data %>%
#'   mutate(top_f2 = make_dicho(top)) %>%
#'   get_freqs(trad_n, top_f2, wts)
#'
#'
#'
#'
#' @export
#'
#'

get_freqs <- function(data, x, group, wt, cross_tab = FALSE) {

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


  if (!missing(group)) {
    # if group is not missing group the data

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

      # convert to a factor with make_factor
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
        dplyr::mutate(group_f = as.factor(group)) %>%
        # group by group_f
        dplyr::group_by(group_f)

    }

  }

  if (missing(wt)) {
    # if missing wt

    # calculate the frequencies
    data_freq <- data %>%
      tidyr::drop_na(dplyr::all_of(x)) %>%
      dplyr::count(.data[[x]]) %>%
      dplyr::mutate(pct = prop.table(n))

    if (!missing(group)) {
      # if not missing group

      # rename the group_f variable with the name supplied in "group"
      data_freq <- data_freq %>% dplyr::rename({{ group }} := group_f)

      if (isTRUE(cross_tab)) {
        # cross_tab is TRUE then pivot the table

        data_freq <- data_freq %>%
          dplyr::mutate(
            pct = make_percent(pct),
            pct_lab = glue::glue("{pct} (n = {n})"),
          ) %>%
          dplyr::select(-c(pct, n)) %>%
          tidyr::pivot_wider(
            names_from = {{ group }},
            values_from = pct_lab
          ) %>%
          dplyr::arrange(.data[[x]])

      }

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

    # calculate the frequencies
    data_freq <- data %>%
      tidyr::drop_na(all_of(x)) %>%
      dplyr::count(.data[[x]], wt = .data[[wt]]) %>%
      dplyr::mutate(pct = prop.table(n),
                    n = round(n, 1))


    if (!missing(group)) {
      # if not missing group

      # rename the group_f variable with the name supplied in "group"
      data_freq <- data_freq %>% dplyr::rename({{ group }} := group_f)

      if (isTRUE(cross_tab)) {
        # cross_tab is TRUE then pivot the table

        data_freq <- data_freq %>%
          dplyr::mutate(
            n = round(n, 1),
            pct = make_percent(pct),
            pct_lab = glue::glue("{pct} (n = {n})"),
          ) %>%
          dplyr::select(-c(pct, n)) %>%
          tidyr::pivot_wider(
            names_from = {{ group }},
            values_from = pct_lab
          ) %>%
          dplyr::arrange(.data[[x]])

      }

    }

  }

  class_names <- class(data_freq)

  data_freq %>% structure(class = c("adlgraphs_freqs", class_names))

}







