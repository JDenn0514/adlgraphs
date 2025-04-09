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
#' @param show_genpop Logical. If the data is grouped, determines if data should 
#'   should be shown for the general population as well. `FALSE`, the default, 
#'   does not show the results for the general population. `TRUE` shows the 
#'   results for the general population in a new column.
#'
#' @export
prettytable <- function(x, show_genpop = FALSE) {
  UseMethod("prettytable")
}

#TODO add a wide argument
#' @export
prettytable.adlgraphs_freqs <- function(x, show_genpop = FALSE) {

  # get the group_names
  group_names <- attr(x, "group_names")
  # get the group labels
  group_labels <- rev(attr_var_label(x[group_names]))
  
  # get teh variable name
  variable_name <- as.character(rlang::quo_squash(attr(x, "variable_name")))
  # get the variable label
  variable_label <- as.character(rlang::quo_squash(attr(x, "variable_label")))

  if (length(group_names) != 0) {

    # clean up the percent column by making it a percentage
    x$pct <- make_percent(x$pct)
    # clean up the n
    x$n <- round(x$n)

    x_wide <- x %>% 
      # pivot the data based on the 
      tidyr::pivot_wider(
        names_from = tidyselect::all_of(group_names),
        values_from = c(pct, n)
      ) 
  
    # create a data.frame with the column names in the right order
    sorted <- sort_cols(x_wide, group_names, variable_name, x)
    

    # put the column names back together as a vector with "_" separating each part
    reordered <- do.call(paste, c(sorted, sep = "_"))
    # reorder the columns using the reorderd vector of col names
    x_wide <- x_wide[c(variable_name, reordered)]

    # reorder the columns in the sorted data
    sorted <- sorted[,c(group_names, "pct_n")]
    # put the col names back together as a vector with the "_" separating each part
    fixed_cols <- do.call(paste, c(sorted, sep = "_"))
    
    # rename the columns in data using fixed_cols
    names(x_wide) <- c(variable_name, fixed_cols)

    if (show_genpop) {
      # get the original data set but remove the groups
      data <- dplyr::ungroup(attr(x, "dataset"))
      # get the variable name
      var_name <- names(data)[1]
      # get the name of the weights variable
      wt <- names(data)[ncol(data)]
      # get the frequencies of the general population
      genpop <- funky_freqs(data, {{ var_name }}, wt = {{ wt }})
      # convert pct to a percent
      genpop$pct <- make_percent(genpop$pct)
      # round the number of respondents
      genpop$n <- round(genpop$n)
      # rename the last two columns
      names(genpop)[names(genpop) %in% c("n", "pct")] <- c(" _General Population_n", " _General Population_pct")
      # reorder the columns
      genpop <- genpop[c(" _General Population_pct", " _General Population_n")]
      # combine the genpop data with the grouped data
      x_wide <- dplyr::bind_cols(x_wide, genpop)
    }
    
    # figure out how many columns there 
    num_cols <- length(names(x_wide))
      
    # create the gt object
    out <- x_wide %>% 
      gt::gt(rowname_col = variable_name)  %>% 
      gt::tab_spanner_delim(delim = "_", split = "first") %>% 
      # fix the column labels for n and pct
      gt::cols_label(.list = fix_pct(.)) %>% 
      # add the title
      gt::tab_header(paste0('Calculating the frequencies for "', variable_label, '"')) %>% 
      # add lines around the whole table
      gt::opt_table_lines("all") %>% 
      gt::tab_style(
        gt::cell_borders(color = "#D3D3D3"),
        gt::cells_column_spanners()
      )
    
    
    
    # get the spanners info
    span <- out[["_spanners", exact = TRUE]]
    # return(span)

    # add the footnotes
    for (x in seq(length(group_names))) {
      # run a for loop where we add a footnote for each group
      # length() gets the number of groups
      # seq() creates the vector of numbers from 1 to the number of groups

      # get the vector of spanner_ids
      span_lvl <- span[span$spanner_level == x,][["spanner_id"]]

      if (show_genpop) {
        # if show_genpop is TRUE, 

        # add a separator column before the genpop pct column
        out <- gt::cols_add(out, " " = "", .before = " _General Population_pct")
        # remove the general population from the vector of footnotes
        span_lvl <- span_lvl[!grepl("General Population", span_lvl, fixed = TRUE)]
      }

      out <- out %>% 
        # add the footnote
        gt::tab_footnote(
          # specify which object from group_labels to use as the footnote 
          footnote = group_labels[x],
          # specify where the footnote is to appear
          locations = gt::cells_column_spanners(
            # specify the spanners to add the footnote to using the span_lvl vector
            spanners = span_lvl,
            # specify the levels
            levels = x
          )
        )
    }

  } else {
    # if there are no groups

    # reorder the columns 
    x <- x[,c(variable_name, "pct", "n")]

    # clean up the pct column by making it a percentage
    x$pct <- make_percent(x$pct)
    # specify the label
    attr(x$pct, "label") <- "Percent"
    # round the number of observations
    x$n <- round(x$n)
    # add the label
    attr(x$n, "label") <- "N"

    # create the object to output
    out <- x %>% 
      # use the variable_name obj to specify the column to use as row labels in the table stub
      gt::gt(rowname_col = variable_name) %>% 
      # add the title
      gt::tab_header(paste0('Calculating the frequencies for "', variable_label, '"')) %>% 
      # add lines around the whole table
      gt::opt_table_lines("all") %>% 
      # specify additional style changes (add cell borders to tab spanners)
      gt::tab_style(
        # specify what style change to do, add cell borders and specify the color
        style = gt::cell_borders(color = "#D3D3D3"),
        # specify where to apply the changes, in the column spanners
        location = gt::cells_column_spanners()
      )
  }

  out
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


