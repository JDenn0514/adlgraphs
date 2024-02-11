#' This mainly an internal package but can be used externally
#'
#' @keywords internal
#' @export
write_word_table <- function(x, doc) {
  doc %>%
    gto::body_add_gt(value = x) %>%
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
#' @param x Either a character string or symbol. The variable with which want
#'   to get the frequencies.
#' @param group Either a character string or a symbol. The grouping variable.
#' @param wt Weights. Add if you have a weighting variable and want to get
#'   weighted frequencies
#'
#' @export
get_freq_table <- function(df, x, group, wt) {

  # use enexpr() to capture the expressions supplied in "x"
  # enexpr returns a naked expression of the argument supplied in "x"
  # this is what allows the input to be either a string or a symbol
  x <- rlang::enexpr(x)
  # if the object supplied in "x" is not a character...
  if (!is.character(x)) {
    # capture x and convert to a symbol object with ensym()
    #then use as_name() to make it a string
    x <- rlang::as_name(rlang::ensym(x))
  }


  if (missing(wt)) {

    if (missing(group)) {
      df %>%
        tidyr::drop_na({{ x }}) %>%
        dplyr::mutate(x_f := haven::as_factor(.data[[x]])) %>%
        dplyr::count(x_f) %>%
        dplyr::mutate(
          pct = prop.table(n),
          pct = round(pct, 3),
          pct = scales::percent(pct, accuracy = 0.1)
        ) %>%
        gt::gt() %>%
        gt::cols_label(
          x_f = x_label(df[[x]]),
          n = "N",
          pct = "Percent"
        )

    } else  {

      # "Returns a naked expression of the variable"
      group <- rlang::enexpr(group)
      if (!is.character(group)) {
        group <- rlang::as_name(rlang::ensym(group))
      }

      # get the variable label
      if (is.null(labelled::var_label(df[[group]]))) {
        x_label <- x
      } else {

      }
      group_label <-  labelled::var_label(df[[group]])
      group_cols <-  c(forcats::fct_unique(df[[group]]))

      # get genpop stats
      genpop <- df %>%
        tidyr::drop_na({{ x }}) %>%
        dplyr::mutate(x_f := haven::as_factor(.data[[x]])) %>%
        dplyr::count(x_f) %>%
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
        tidyr::drop_na({{ x }}, {{ group }}) %>%
        dplyr::mutate(x_f := haven::as_factor(.data[[x]])) %>%
        group_by(!!sym({{ group }})) %>%
        dplyr::count(x_f) %>%
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
        dplyr::select(-x_f)


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
            x_f,
            `General Population`,
            utils::tail(group_cols, n = 1)
          ),
          color = "gray80"
        ) %>%
        gt::cols_label(
          x_f = var_label(df[[x]])
        )

    }

  } else {
    if (missing(group)) {
      df %>%
        tidyr::drop_na({{ x }}) %>%
        dplyr::mutate(x_f := haven::as_factor(.data[[x]])) %>%
        dplyr::count(x_f, wt = !!sym({{ wt }} )) %>%
        dplyr::mutate(
          pct = prop.table(n),
          n = round(n),
          pct = scales::percent(pct, accuracy = 0.1)
        ) %>%
        gt::gt() %>%
        gt::cols_label(
          x_f = var_label(df[[x]]),
          n = "N",
          pct = "Percent"
        )


    } else {

      group_label <-  labelled::var_label(df[[group]])
      group_cols <-  c(forcats::fct_unique(df[[group]]))

      # get genpop stats
      genpop <- df %>%
        tidyr::drop_na({{ x }}) %>%
        dplyr::mutate(x_f := haven::as_factor(.data[[x]])) %>%
        dplyr::count(x_f, wt = !!sym({{ wt }} )) %>%
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
        tidyr::drop_na({{ x }}, {{ group }}) %>%
        dplyr::mutate(x_f := haven::as_factor(.data[[x]])) %>%
        dplyr::group_by(!!sym({{ group }})) %>%
        dplyr::count(x_f, wt = !!sym({{ wt }} )) %>%
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
        dplyr::select(-x_f)


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
            x_f,
            `General Population`,
            tail(group_cols, n = 1)
          ),
          color = "gray80"
        ) %>%
        gt::cols_label(
          x_f = var_label(df[[x]])
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
#' @param x A vector of variables you want to get the frequencies for.
#' @param group A character string. The first grouping variable.
#' @param wt A character string. Add if you have a weighting variable and want
#'   to get weighted frequencies
#' @param file_name A character string specifying the name of the file to be
#'   created with the frequencies and where the file will be located. File must
#'   end in .docx
#'
#' @export
get_all_freqs <- function(df, x, group, wt, file_name) {

  if (missing(wt)) {

    if (missing(group)) {

      # get the number of variables in the x vector
      leng <- length(x)
      # create a list of the dataframes
      df_list <- rep(list(df), leng)
      # get the frequencies
      freqs <- purrr::pmap(list(df = df_list, x = x), get_freq_table)

    } else {

      # get the number of variables in the x vector
      leng <- length(x)
      # create a list of the dataframes
      df_list <- rep(list(df), leng)
      # create a vector of group
      group_list <- replicate(leng, group)
      # get the frequencies
      freqs <- purrr::pmap(list(df = df_list, x = x, group = group_list), get_freq_table)

    }

  } else {

    if (missing(group)) {

      # get the number of variables in the x vector
      leng <- length(x)
      # create a list of the dataframes
      df_list <- rep(list(df), leng)
      # create a vector of the weights
      wt_list <- replicate(leng, wt)
      # get the frequencies
      freqs <- purrr::pmap(list(df = df_list, x = x, wt = wt_list), get_freq_table)

    } else {

      # get the number of variables in the x vector
      leng <- length(x)
      # create a list of the dataframes
      df_list <- rep(list(df), leng)
      # create a vector of group
      group_list <- replicate(leng, group)
      # create a vector of the list
      wt_list <- replicate(leng, wt)
      # get the frequencies
      freqs <- purrr::pmap(list(df = df_list, x = x, group = group_list, wt = wt_list), get_freq_table)

    }

  }


  my_doc <- officer::read_docx()

  # Print the tables
  purrr::walk(freqs, write_word_table, my_doc)
  print(my_doc, target = file_name) %>% invisible()

}


