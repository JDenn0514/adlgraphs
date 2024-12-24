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
prettytable.adlgraphs_freqs <- function(x, wide = TRUE) {

  group_names <- attr(x, "group_names")
  variable_name <- as.character(rlang::quo_squash(attr(x, "variable_name")))


  if (length(group_names) != 0) {
    # clean up the percent column
    x$pct <- make_percent(x$pct)
    # clean up the n
    x$n <- round(x$n)

    data <- x %>% 
      # pivot the data based on the 
      tidyr::pivot_wider(
        names_from = tidyselect::all_of(group_names),
        values_from = c(pct, n)
      ) 
  
    # create a data.frame with the column names in the right order
    sorted <- sort_cols(data, group_names, x)

    # put the column names back together as a vector with "_" separating each part
    reordered <- do.call(paste, c(sorted, sep = "_"))
    # reorder the columns using the reorderd vector of col names
    data <- data[c(variable_name, reordered)]

    # reorder the columns in the sorted data
    sorted <- sorted[,c(group_names, "pct_n")]
    # put the col names back together as a vector with the "_" separating each part
    fixed_cols <- do.call(paste, c(sorted, sep = "_"))

    # rename the columns in data using fixed_cols
    names(data) <- c(variable_name, fixed_cols)
    # return(data)
      
    # create the gt object
    data %>% 
      gt::gt(rowname_col = variable_name)  %>% 
      gt::tab_spanner_delim(delim = "_", split = "first") %>% 
      # fix the column labels for n and pct
      gt::cols_label(.list = fix_pct(.)) %>% 
      gt::tab_header(paste0('Calculating the frequencies for "', attr(x, "variable_label"), '"')) %>% 
      gt::opt_table_lines("all") %>% 
      gt::tab_style(
        gt::cell_borders(color = "#D3D3D3"),
        gt::cells_column_spanners()
      )
  } else {
    # reorder the columns 
    x <- x[,c(variable_name, "pct", "n")]
    # clean up
    x$pct <- make_percent(x$pct)
    attr(x$pct, "label") <- "Percent"
    x$n <- round(x$n)
    attr(x$n, "label") <- "N"
    x %>% 
      gt::gt(rowname_col = variable_name) %>% 
      gt::tab_header(paste0('Calculating the frequencies for "', attr(x, "variable_label"), '"')) %>% 
      gt::opt_table_lines("all") 
  }

}


# prettytable.adlgraphs_dunnett <- function(x) {
#
#   if
#
#   # make a gt table
#   gt(x, groupname_col = group) %>%
#
#     # updat the columns
#     cols_label(
#       treatments = "Treatments",
#       mean = "Mean",
#       sd = "Standard Deviation",
#       n = "N",
#       std.error = "Standard Error",
#       conf.low = "Low CI",
#       conf.high = "High CI"
#     ) %>%
#     text_case_match(
#       NA ~ "",
#       .locations = cells_body(columns = c("ss"))
#     ) %>%
#     cols_hide(ss) %>%
#     gt_highlight_rows(
#       rows = ss == "Yes"
#     ) %>%
#     # add title and subtitle
#     tab_header(
#       # use glue and attr_var_label to use the variable labels
#       title = glue('"{attr_var_label(eah[[dv]])}"'),
#       subtitle = '1 = "Strongly disagree" and 4 = "Strongly agree"'
#     ) %>%
#     tab_source_note(source_note = "Bolded and highlighted rows indicate a statistically significant difference from Placebo") %>%
#     # add an outline
#     opt_table_outline() %>%
#     # add lines for all cells
#     opt_table_lines("all")
#
# }


#prettytable.adlgraphs_means <- function() {

#}


