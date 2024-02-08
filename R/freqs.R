#' This mainly an internal package but can be used externally
#'
#' @keywords internal
#' @export
write_word_table <- function(var, doc) {
  doc %>%
    gto::body_add_gt(value = var) %>%
    officer::body_add_par(value = "") %>%
    officer::body_add_par(value = "")
}


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
#' @param var Either a character string or symbol. The variable with which want
#'   to get the frequencies.
#' @param group Either a character string or a symbol. The grouping variable.
#' @param wt Weights. Add if you have a weighting variable and want to get
#'   weighted frequencies
#'
#' @export
get_freq_table <- function(df, var, group, wt) {

  var <- rlang::enexpr(var)
  group <- rlang::enexpr(group)

  if (!is.character(var)) {
    var <- rlang::as_name(rlang::ensym(var))
  }

  if (!is.character(group)) {
    group <- rlang::as_name(rlang::ensym(group))
  }


  if (missing(wt)) {

    if (missing(group)) {
      df %>%
        tidyr::drop_na({{ var }}) %>%
        dplyr::mutate(var_f := haven::as_factor(.data[[var]])) %>%
        dplyr::count(var_f) %>%
        dplyr::mutate(
          pct = prop.table(n),
          pct = round(pct, 3),
          pct = scales::percent(pct, accuracy = 0.1)
        ) %>%
        gt::gt() %>%
        gt::cols_label(
          var_f = var_label(df[[var]]),
          n = "N",
          pct = "Percent"
        )

    } else  {

      group_label <-  labelled::var_label(df[[group]])
      group_cols <-  c(forcats::fct_unique(df[[group]]))

      # get genpop stats
      genpop <- df %>%
        tidyr::drop_na({{ var }}) %>%
        dplyr::mutate(var_f := haven::as_factor(.data[[var]])) %>%
        dplyr::count(var_f) %>%
        dplyr::mutate(
          pct = prop.table(n),
          pct = round(pct, 3),
          pct = scales::percent(pct, accuracy = 0.1),
          `General Population` = glue("{pct} ({n})") %>%
            stringr::str_replace(" ", "<br>")
        ) %>%
        dplyr::select(-c(n, pct))


      # get stats by age group
      group <- df  %>%
        tidyr::drop_na({{ var }}, {{ group }}) %>%
        dplyr::mutate(var_f := haven::as_factor(.data[[var]])) %>%
        group_by(!!sym({{ group }})) %>%
        dplyr::count(var_f) %>%
        dplyr::mutate(
          pct = prop.table(n),
          pct = round(pct, 3),
          pct = scales::percent(pct, accuracy = 0.1),
          pct = glue("{pct} ({n})") %>%
            stringr::str_replace(" ", "<br>")
        ) %>%
        dplyr::select(-n) %>%
        tidyr::pivot_wider(
          names_from = {{ group }},
          values_from = pct
        ) %>%
        dplyr::select(-var_f)


      dplyr::bind_cols(genpop, group) %>%
        gt::gt() %>%
        gt::tab_spanner(
          label = group_label,
          columns = group_cols
        ) %>%
        gt::fmt_markdown(
          columns = everything()
        ) %>%
        gtExtras::gt_add_divider(
          columns = c(
            var_f,
            `General Population`,
            tail(group_cols, n = 1),
          ),
          color = "gray80"
        ) %>%
        gt::cols_label(
          var_f = var_label(df[[var]])
        )

    }

  } else {
    if (missing(group)) {
      df %>%
        tidyr::drop_na({{ var }}) %>%
        dplyr::mutate(var_f := haven::as_factor(.data[[var]])) %>%
        dplyr::count(var_f, wt = !!sym({{ wt }} )) %>%
        dplyr::mutate(
          pct = prop.table(n),
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1)
        ) %>%
        gt::gt() %>%
        gt::cols_label(
          var_f = var_label(df[[var]]),
          n = "N",
          pct = "Percent"
        )


    } else {

      group_label <-  labelled::var_label(df[[group]])
      group_cols <-  c(forcats::fct_unique(df[[group]]))

      # get genpop stats
      genpop <- df %>%
        tidyr::drop_na({{ var }}) %>%
        dplyr::mutate(var_f := haven::as_factor(.data[[var]])) %>%
        dplyr::count(var_f, wt = !!sym({{ wt }} )) %>%
        dplyr::mutate(
          pct = prop.table(n),
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1),
          `General Population` = glue("{pct} ({n})") %>%
            stringr::str_replace(" ", "<br>")
        ) %>%
        dplyr::select(-c(n, pct))


      # get stats by age group
      group <- df  %>%
        tidyr::drop_na({{ var }}, {{ group }}) %>%
        dplyr::mutate(var_f := haven::as_factor(.data[[var]])) %>%
        dplyr::group_by(!!sym({{ group }})) %>%
        dplyr::count(var_f, wt = !!sym({{ wt }} )) %>%
        dplyr::mutate(
          pct = prop.table(n),
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1),
          pct = glue("{pct} ({n})") %>%
            stringr::str_replace(" ", "<br>")
        ) %>%
        dplyr::select(-n) %>%
        tidyr::pivot_wider(
          names_from = {{ group }},
          values_from = pct
        ) %>%
        dplyr::select(-var_f)


      dplyr::bind_cols(genpop, group) %>%
        gt::gt() %>%
        gt::tab_spanner(
          label = group_label,
          columns = group_cols
        ) %>%
        gt::fmt_markdown(
          columns = everything()
        ) %>%
        gtExtras::gt_add_divider(
          columns = c(
            var_f,
            `General Population`,
            tail(group_cols, n = 1),
          ),
          color = "gray80"
        ) %>%
        gt::cols_label(
          var_f = var_label(df[[var]])
        )
    }

  }


}



#' Export frequencies for a set of variables to a word doc.
#'
#' This function uses [get_freq_table()] to get the frequencies for a set of variables
#' suppplied by the user. It then outputs these frequencies to a word doc.
#'
#' @param df An object of type data.frame or tibble. If piping the df into the
#'   function, this is not required.
#' @param var A vector of variables you want to get the frequencies for.
#' @param group A character string. The first grouping variable.
#' @param wt A character string. Add if you have a weighting variable and want
#'   to get weighted frequencies
#'
#' @export
get_all_freqs <- function(df, var, group, wt, file_name) {

  if (missing(wt)) {

    if (missing(group)) {

      # get the number of variables in the var vector
      leng <- length(var)
      # create a list of the dataframes
      df_list <- rep(list(df), leng)
      # get the frequencies
      freqs <- purrr::pmap(list(df = df_list, var = var), get_freq_table)

    } else {

      # get the number of variables in the var vector
      leng <- length(var)
      # create a list of the dataframes
      df_list <- rep(list(df), leng)
      # create a vector of group
      group_list <- replicate(leng, group)
      # get the frequencies
      freqs <- purrr::pmap(list(df = df_list, var = var, group = group_list), get_freq_table)

    }

  } else {

    if (missing(group)) {

      # get the number of variables in the var vector
      leng <- length(var)
      # create a list of the dataframes
      df_list <- rep(list(df), leng)
      # create a vector of the weights
      wt_list <- replicate(leng, wt)
      # get the frequencies
      freqs <- purrr::pmap(list(df = df_list, var = var, wt = wt_list), get_freq_table)

    } else {

      # get the number of variables in the var vector
      leng <- length(var)
      # create a list of the dataframes
      df_list <- rep(list(df), leng)
      # create a vector of group
      group_list <- replicate(leng, group)
      # create a vector of the list
      wt_list <- replicate(leng, wt)
      # get the frequencies
      freqs <- purrr::pmap(list(df = df_list, var = var, group = group_list, wt = wt_list), get_freq_table)

    }

  }


  my_doc <- officer::read_docx()

  # Print the tables
  purrr::walk(freqs, write_word_table, my_doc)
  print(my_doc, target = file_name) %>% invisible()

}


