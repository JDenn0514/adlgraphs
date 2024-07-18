#' Get the frequencies as a GT table
#'
#' This function creates a GT table of the frequencies of a specified variable
#' and has the ability to get the frequencies for one grouping variable.
#' While this can be used on it's own, it was created more to be used in
#' [get_all_freqs()]`, a function that outputs the frequencies of a set of
#' variables to a word doc.
#'
#' @param df An object of type data.frame or tibble. If piping the df into the
#'   function, this is not required.
#' @param x Either a character string or symbol. The variable with which want
#'   to get the frequencies.
#' @param group Either a character string or a symbol. The grouping variable.
#' @param wt Weights. Add if you have a weighting variable and want to get
#'   weighted frequencies
#' @param show_genpop Logical. Should there be a column showing the frequencies
#'   for the general population
#'
#' @export
get_freq_table <- function(df, x, group, wt, show_genpop = FALSE) {

  # get the x object's name
  x_lab <- deparse(substitute(x))

  # convert strings or symbols to strings
  x <- accept_string_or_sym({{ x }})

  # set the x variable label
  x_variable_label <- get_variable_label(df[[x]], x_lab)


  if (missing(wt)) {
    # if wt is missing, calculate freqs without weights

    if (missing(group)) {
      # if group is missing calculate simple freqs

      # get the frequencies without weights and without a grouping variable
      df %>%
        dplyr::mutate(x_f = make_factor(.data[[x]])) %>%
        get_freqs(x_f) %>%
        dplyr::mutate(pct = make_percent(pct)) %>%
        gt::gt() %>%
        gt::cols_label(
          x_f = x_variable_label,
          n = "N",
          pct = "Percent"
        )

    } else {
      # if group is not missing, calculate frequencies by group

      # get the group object's name
      group_lab <- deparse(substitute(group))

      # convert strings or symbols to strings
      group <- accept_string_or_sym({{ group }})

      # get the group column labels
      group_cols <- get_unique_labels(df[[group]])

      # set the group variable label
      group_variable_label <- get_variable_label(df[[group]], group_lab)


      if (isFALSE(show_genpop)) {
        # if show_genpop = FALSE don't show the overall frequencies

        df %>%
          dplyr::mutate(x_f = make_factor(.data[[x]])) %>%
          get_freqs(x_f, {{ group }}, cross_tab = TRUE) %>%
          gt::gt() %>%
          gt::cols_label(x_f = x_variable_label) %>%
          gt::tab_spanner(
            label = group_variable_label,
            columns = group_cols
          )

      } else {
        # if show_genpop = TRUE show the frequencies for the general population

        genpop_df <- df %>%
          # convert to a factor (this even works with numeric variables)
          dplyr::mutate(x_f = make_factor(.data[[x]])) %>%
          # calculate the frequencies
          get_freqs(x_f) %>%
          # data transforming
          dplyr::mutate(
            # fix the percents
            pct_lab = make_percent(pct),
            # make a new General Population column with pct and n
            `General Population` = glue("{pct_lab} (n = {n})")
          ) %>%
          # remove n, pct, and pct_lab
          dplyr::select(-c(n:pct_lab))

        group_df <- df %>%
          # convert to a factor (this even works with numeric variables)
          mutate(x_f = make_factor(.data[[x]])) %>%
          # calculate frequencies for x_f with a group and make it cross tabs
          get_freqs(x_f, {{group}}, cross_tab = TRUE) %>%
          # remove x_f
          select(-x_f)

        bind_cols(genpop_df, group_df) %>%
          # convert to a gt_object
          gt::gt() %>%
          gt::tab_spanner(
            label = group_variable_label,
            columns = group_cols
          ) %>%
          gt_add_divider(
            columns = c(
              x_f,
              `General Population`,
              utils::tail(group_cols, n = 1)
            ),
            color = "gray80"
          ) %>%
          gt::cols_label(
            x_f = x_variable_label
          )

      }


    }

  } else {
    # if not wt is present calculate frequencies with weights

    wt <- accept_string_or_sym({{ wt }})

    if (missing(group)) {
      # if group is missing calculate simple freqs

      # get the frequencies without weights and without a grouping variable
      df %>%
        dplyr::mutate(x_f = make_factor(.data[[x]])) %>%
        get_freqs(x_f, wt = {{ wt }}) %>%
        dplyr::mutate(pct = make_percent(pct)) %>%
        gt::gt() %>%
        gt::cols_label(
          x_f = x_variable_label,
          n = "N",
          pct = "Percent"
        )

    } else {
      # if group is not missing, calculate frequencies by group

      # get the group object's name
      group_lab <- deparse(substitute(group))

      # convert strings or symbols to strings
      group <- accept_string_or_sym({{ group }})

      # get the group column labels
      group_cols <- get_unique_labels(df[[group]])

      # set the group variable label
      group_variable_label <- get_variable_label(df[[group]], group_lab)


      if (isFALSE(show_genpop)) {
        # if show_genpop = FALSE don't show the overall frequencies

        df %>%
          dplyr::mutate(x_f = make_factor(.data[[x]])) %>%
          get_freqs(x_f, {{ group }}, {{ wt }}, cross_tab = TRUE) %>%
          gt::gt() %>%
          gt::cols_label(x_f = x_variable_label) %>%
          gt::tab_spanner(
            label = group_variable_label,
            columns = group_cols
          )

      } else {
        # if show_genpop = TRUE show the frequencies for the general population

        genpop_df <- df %>%
          # convert to a factor (this even works with numeric variables)
          dplyr::mutate(x_f = make_factor(.data[[x]])) %>%
          # calculate the frequencies
          get_freqs(x_f, wt = {{ wt }}) %>%
          # data transforming
          dplyr::mutate(
            # fix the percents
            pct_lab = make_percent(pct),
            # make a new General Population column with pct and n
            `General Population` = glue("{pct_lab} (n = {n})")
          ) %>%
          # remove n, pct, and pct_lab
          dplyr::select(-c(n:pct_lab))

        group_df <- df %>%
          # convert to a factor (this even works with numeric variables)
          mutate(x_f = make_factor(.data[[x]])) %>%
          # calculate frequencies for x_f with a group and make it cross tabs
          get_freqs(x_f, {{group}}, {{ wt }}, cross_tab = TRUE) %>%
          # remove x_f
          select(-x_f)

        bind_cols(genpop_df, group_df) %>%
          # convert to a gt_object
          gt::gt() %>%
          gt::tab_spanner(
            label = group_variable_label,
            columns = group_cols
          ) %>%
          gt_add_divider(
            columns = c(
              x_f,
              `General Population`,
              utils::tail(group_cols, n = 1)
            ),
            color = "gray80"
          ) %>%
          gt::cols_label(
            x_f = x_variable_label
          )

      }

    }

  }

}

