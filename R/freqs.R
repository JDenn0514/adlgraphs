#' Internal function to be used when we export to a word doc
write_word_table <- function(var, doc) {
  doc %>%
    gto::body_add_gt(value = var) %>%
    officer::body_add_par(value = "") %>%
    officer::body_add_par(value = "")
}


#' Get the frequencies as a GT table
#'
#' This function creates a GT table of the frequencies of a specified variable
#' and has the ability to get the frequencies for up to two grouping variables.
#' While this can be used on it's own, it was created more to be used in
#' `get_all_freqs`, a function that outputs the frequencies of a set of
#' variables to a word doc.
#'
#' @param df An object of type data.frame or tibble. If piping the df into the
#'   function, this is not required.
#' @param var A character vector. The variable with which want to get the
#'   frequencies.
#' @param group1 A character string The first grouping variable.
#' @param group2 A character string The second grouping variable.
#' @param wt Weights. Add if you have a weighting variable and want to get
#'   weighted frequencies
#'
#' @export
freq_fun <- function(df, var, group1, group2, wt) {

  if (missing(wt)) {

    if (missing(group1) && missing(group2)) {
      df %>%
        tidyr::drop_na({{ var }}) %>%
        dplyr::mutate(var_f := haven::as_factor(!!sym({{ var }}))) %>%
        socsci::ct(var_f) %>%
        dplyr::mutate(
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1)
        ) %>%
        gt::gt()

    } else if (missing(group2)) {

      group1_label <-  labelled::var_label(df[[group1]])
      group1_cols <-  c(forcats::fct_unique(df[[group1]]))

      # get genpop stats
      genpop <- df %>%
        tidyr::drop_na({{ var }}) %>%
        dplyr::mutate(var_f := haven::as_factor(!!sym({{ var }}))) %>%
        socsci::ct(var_f) %>%
        dplyr::mutate(
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1),
          `General Population` = glue("{pct} (n = {n})") %>%
            stringr::str_replace(" ", "<br>")
        ) %>%
        dplyr::select(-c(n, pct))


      # get stats by age group
      group1 <- df  %>%
        tidyr::drop_na({{ var }}, {{ group1 }}) %>%
        dplyr::mutate(var_f := haven::as_factor(!!sym({{ var }}))) %>%
        group_by(!!sym({{ group1 }})) %>%
        socsci::ct(var_f) %>%
        dplyr::mutate(
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1),
          pct = glue("{pct} (n = {n})") %>%
            stringr::str_replace(" ", "<br>")
        ) %>%
        dplyr::select(-n) %>%
        tidyr::pivot_wider(
          names_from = {{ group1 }},
          values_from = pct
        ) %>%
        dplyr::select(-var_f)


      dplyr::bind_cols(genpop, group1) %>%
        gt::gt() %>%
        gt::tab_spanner(
          label = group1_label,
          columns = group1_cols
        ) %>%
        gt::fmt_markdown(
          columns = everything()
        ) %>%
        gtExtras::gt_add_divider(
          columns = c(
            var_f,
            `General Population`
          ),
          color = "gray80"
        )
    } else {

      group1_label <-  labelled::var_label(df[[group1]])
      group1_cols <-  c(forcats::fct_unique(df[[group1]]))
      group2_label <-  labelled::var_label(df[[group2]])
      group2_cols <-  c(forcats::fct_unique(df[[group2]]))

      # get genpop stats
      genpop <- df  %>%
        tidyr::drop_na({{ var }}) %>%
        dplyr::mutate(var_f := haven::as_factor(!!sym({{ var }}))) %>%
        socsci::ct(var_f) %>%
        dplyr::mutate(
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1),
          `General Population` = glue("{pct} (n = {n})") %>%
            stringr::str_replace(" ", "<br>")
        ) %>%
        dplyr::select(-c(n, pct))


      # get stats by age group
      group1 <- df  %>%
        tidyr::drop_na({{ var }}, {{ group1 }}) %>%
        dplyr::mutate(var_f := haven::as_factor(!!sym({{ var }}))) %>%
        group_by(!!sym({{ group1 }})) %>%
        socsci::ct(var_f) %>%
        dplyr::mutate(
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1),
          pct = glue("{pct} (n = {n})") %>%
            stringr::str_replace(" ", "<br>")
        ) %>%
        dplyr::select(-n) %>%
        tidyr::pivot_wider(
          names_from = {{ group1 }},
          values_from = pct
        ) %>%
        dplyr::select(-var_f)

      # get stats by age group
      group2 <- df  %>%
        tidyr::drop_na({{ var }}, {{ group2 }}) %>%
        dplyr::mutate(var_f := haven::as_factor(!!sym({{ var }}))) %>%
        group_by(!!sym({{ group2 }})) %>%
        socsci::ct(var_f) %>%
        dplyr::mutate(
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1),
          pct = glue("{pct} (n = {n})") %>%
            stringr::str_replace(" ", "<br>")
        ) %>%
        dplyr::select(-n) %>%
        tidyr::pivot_wider(
          names_from = {{ group2 }},
          values_from = pct
        ) %>%
        dplyr::select(-var_f)


      dplyr::bind_cols(genpop, group1, group2) %>%
        #relocate(`General Population`, .after = variable) %>%
        gt::gt() %>%
        gt::tab_spanner(
          label = group1_label,
          columns = group1_cols
        ) %>%
        gt::tab_spanner(
          label = group2_label,
          columns = group2_cols
        ) %>%
        gt::fmt_markdown(
          columns = everything()
        ) %>%
        gtExtras::gt_add_divider(
          columns = c(
            var_f,
            `General Population`,
            tail(group1_cols, n = 1),
          ),
          color = "gray80"
        )
    }

  } else {
    if (missing(group1) && missing(group2)) {
      df %>%
        tidyr::drop_na({{ var }}) %>%
        dplyr::mutate(var_f := haven::as_factor(!!sym({{ var }}))) %>%
        socsci::ct(var_f, wt = !!sym({{ wt }} )) %>%
        dplyr::mutate(
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1)
        ) %>%
        gt::gt()

    } else if (missing(group2)) {

      group1_label <-  labelled::var_label(df[[group1]])
      group1_cols <-  c(forcats::fct_unique(df[[group1]]))

      # get genpop stats
      genpop <- df %>%
        tidyr::drop_na({{ var }}) %>%
        dplyr::mutate(var_f := haven::as_factor(!!sym({{ var }}))) %>%
        socsci::ct(var_f, wt = !!sym({{ wt }} )) %>%
        dplyr::mutate(
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1),
          `General Population` = glue("{pct} (n = {n})") %>%
            stringr::str_replace(" ", "<br>")
        ) %>%
        dplyr::select(-c(n, pct))


      # get stats by age group
      group1 <- df  %>%
        tidyr::drop_na({{ var }}, {{ group1 }}) %>%
        dplyr::mutate(var_f := haven::as_factor(!!sym({{ var }}))) %>%
        group_by(!!sym({{ group1 }})) %>%
        socsci::ct(var_f, wt = !!sym({{ wt }} )) %>%
        dplyr::mutate(
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1),
          pct = glue("{pct} (n = {n})") %>%
            stringr::str_replace(" ", "<br>")
        ) %>%
        dplyr::select(-n) %>%
        tidyr::pivot_wider(
          names_from = {{ group1 }},
          values_from = pct
        ) %>%
        dplyr::select(-var_f)


      dplyr::bind_cols(genpop, group1) %>%
        gt::gt() %>%
        gt::tab_spanner(
          label = group1_label,
          columns = group1_cols
        ) %>%
        gt::fmt_markdown(
          columns = everything()
        ) %>%
        gtExtras::gt_add_divider(
          columns = c(
            var_f,
            `General Population`
          ),
          color = "gray80"
        )
    } else {

      group1_label <-  labelled::var_label(df[[group1]])
      group1_cols <-  c(forcats::fct_unique(df[[group1]]))
      group2_label <-  labelled::var_label(df[[group2]])
      group2_cols <-  c(forcats::fct_unique(df[[group2]]))

      # get genpop stats
      genpop <- df  %>%
        tidyr::drop_na({{ var }}) %>%
        dplyr::mutate(var_f := haven::as_factor(!!sym({{ var }}))) %>%
        socsci::ct(var_f) %>%
        dplyr::mutate(
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1),
          `General Population` = glue("{pct} (n = {n})") %>%
            stringr::str_replace(" ", "<br>")
        ) %>%
        dplyr::select(-c(n, pct))


      # get stats by age group
      group1 <- df  %>%
        tidyr::drop_na({{ var }}, {{ group1 }}) %>%
        dplyr::mutate(var_f := haven::as_factor(!!sym({{ var }}))) %>%
        group_by(!!sym({{ group1 }})) %>%
        socsci::ct(var_f, wt = !!sym({{ wt }} )) %>%
        dplyr::mutate(
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1),
          pct = glue("{pct} (n = {n})") %>%
            stringr::str_replace(" ", "<br>")
        ) %>%
        dplyr::select(-n) %>%
        tidyr::pivot_wider(
          names_from = {{ group1 }},
          values_from = pct
        ) %>%
        dplyr::select(-var_f)

      # get stats by age group
      group2 <- df  %>%
        tidyr::drop_na({{ var }}, {{ group2 }}) %>%
        dplyr::mutate(var_f := haven::as_factor(!!sym({{ var }}))) %>%
        group_by(!!sym({{ group2 }})) %>%
        socsci::ct(var_f, wt = !!sym({{ wt }} )) %>%
        dplyr::mutate(
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1),
          pct = glue("{pct} (n = {n})") %>%
            stringr::str_replace(" ", "<br>")
        ) %>%
        dplyr::select(-n) %>%
        tidyr::pivot_wider(
          names_from = {{ group2 }},
          values_from = pct
        ) %>%
        dplyr::select(-var_f)


      dplyr::bind_cols(genpop, group1, group2) %>%
        #relocate(`General Population`, .after = variable) %>%
        gt::gt() %>%
        gt::tab_spanner(
          label = group1_label,
          columns = group1_cols
        ) %>%
        gt::tab_spanner(
          label = group2_label,
          columns = group2_cols
        ) %>%
        gt::fmt_markdown(
          columns = everything()
        ) %>%
        gtExtras::gt_add_divider(
          columns = c(
            var_f,
            `General Population`,
            tail(group1_cols, n = 1),
          ),
          color = "gray80"
        )
    }

  }


}


#' Export frequencies for a set of variables to a word doc.
#'
#' This function uses `freq_fun` to get the frequencies for a set of variables
#' suppplied by the user. It then outputs these frequencies to a word doc.
#'
#' @param df An object of type data.frame or tibble. If piping the df into the
#'   function, this is not required.
#' @param var A vector of variables you want to get the frequencies for.
#' @param group1 A character string. The first grouping variable.
#' @param group2 A character string. The second grouping variable.
#' @param wt A character string. Add if you have a weighting variable and want
#'   to get weighted frequencies
#'
#' @export
get_all_freqs <- function(df, var, group1, group2, wt, file_name) {

  if (missing(wt)) {

    if (missing(group1) && missing(group2)) {

      # get the number of variables in the var vector
      leng <- length(var)
      # create a list of the dataframes
      df_list <- rep(list(df), leng)
      # get the frequencies
      freqs <- purrr::pmap(list(df_list, var), freq_fun)

    } else if (missing(group2)) {

      # get the number of variables in the var vector
      leng <- length(var)
      # create a list of the dataframes
      df_list <- rep(list(df), leng)
      # create a vector of group1
      group1_list <- replicate(leng, group1)
      # get the frequencies
      freqs <- purrr::pmap(list(df_list, var, group1_list), freq_fun)

    } else {

      # get the number of variables in the var vector
      leng <- length(var)
      # create a list of the dataframes
      df_list <- rep(list(df), leng)
      # create a vector of group1
      group1_list <- replicate(leng, group1)
      # create a vector of group2
      group2_list <- replicate(leng, group2)
      # get the frequencies
      freqs <- purrr::pmap(list(df_list, var, group1_list, group2_list), freq_fun)

    }

  } else {

    if (missing(group1) && missing(group2)) {

      # get the number of variables in the var vector
      leng <- length(var)
      # create a list of the dataframes
      df_list <- rep(list(df), leng)
      # create a vector of the weights
      wt_list <- replicate(leng, wt)
      # get the frequencies
      freqs <- purrr::pmap(list(df_list, var), freq_fun)

    } else if (missing(group2)) {

      # get the number of variables in the var vector
      leng <- length(var)
      # create a list of the dataframes
      df_list <- rep(list(df), leng)
      # create a vector of group1
      group1_list <- replicate(leng, group1)
      # create a vector of the list
      wt_list <- replicate(leng, wt)
      # get the frequencies
      freqs <- purrr::pmap(list(df_list, var, group1_list), freq_fun)

    } else {

      # get the number of variables in the var vector
      leng <- length(var)
      # create a list of the dataframes
      df_list <- rep(list(df), leng)
      # create a vector of group1
      group1_list <- replicate(leng, group1)
      # create a vector of group2
      group2_list <- replicate(leng, group2)
      # create a vector of wt
      wt_list <- replicate(leng, wt)
      # get the frequencies
      freqs <- purrr::pmap(list(df_list, var, group1_list, group2_list), freq_fun)

    }

  }


  my_doc <- offcer::read_docx()

  # Print the tables
  purrr::walk(freqs, write_word_table, my_doc)
  print(my_doc, target = file_name) %>% invisible()

}
