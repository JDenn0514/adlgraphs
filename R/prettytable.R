#' Make pretty HTML tables
#'
#' Takes a data frame object and makes it into a pretty table. This is designed
#' to work within the `adlgraphs` package but it should work with most objects
#' of class `tbl_df`, `tbl`, or `data.frame`. Very important note, at the moment,
#' this only works with frequency tables that have columns labelled `pct` and
#' `n`. I do not see this changing for frequency tables. Nevertheless, I am
#' working on adding in more functionality, including for mean tables, tables of
#' factor loadings, linear regression coefficients, and others.
#'
#' @param x An object to turn into a pretty table.
#'
#'
#' @export
prettytable <- function(x) {
  UseMethod("prettytable")
}

#' @export
# create the default pretty table function
prettytable.default <- function(x) {

  # by specifying any
  if ("pct" %in% colnames(x)) {
    prettytable.adlgraphs_freqs(x)
  }
}

#' @export
prettytable.adlgraphs_freqs <- function(x) {

  num_cols <- length(colnames(x))

  if (num_cols == 3) {
    # if there are only three columns it means there is no grouping variable

    # get the name of the main variable
    x_lab <- colnames(x)[1]

    # get the label of the main variable
    x_variable_label <- get_var_label(x[[x_lab]], x_lab)

    x %>%
      dplyr::mutate(
        pct = make_percent(pct),
        n = round(n, 2),
        x_f = make_factor(.data[[x_lab]])
      ) %>%
      select(x_f, n, pct) %>%
      gt::gt() %>%
      gt::cols_label(
        x_f = x_variable_label,
        n = "N",
        pct = "Percent"
      )

  } else if (num_cols == 4) {

    # get the name of the main variable
    x_lab <- colnames(x)[2]
    # get the label of the main variable
    x_variable_label <- get_var_label(x[[x_lab]], x_lab)

    # get the group object's name
    group_lab <- colnames(x)[1]
    # get the group column labels
    group_cols <- get_unique_labels(x[[1]])
    # set the group variable label
    group_variable_label <- get_var_label(x[[group_lab]], group_lab)

    x %>%
      dplyr::mutate(
        pct = make_percent(pct),
        n = round(n, 2),
        pct_lab = glue::glue("{pct} (n = {n})"),
        x_f = make_factor(.data[[x_lab]])
      ) %>%
      dplyr::select(c(x_f, dplyr::all_of({{ group_lab }}), pct_lab)) %>%
      tidyr::pivot_wider(
        names_from = group_lab,
        values_from = pct_lab
      ) %>%
      gt::gt() %>%
      gt::cols_label(x_f = x_variable_label) %>%
      gt::tab_spanner(
        label = group_variable_label,
        columns = group_cols
      )

  }

}




#prettytable.adlgraphs_means <- function() {

#}


