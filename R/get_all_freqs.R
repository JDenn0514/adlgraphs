#' Export frequencies for a set of variables to a word doc.
#'
#' This function makes it easy to export frequencies and cross-tabs of a set
#'   of variables to a Word Document. It uses the 
#'
#' @param data A data frame or tibble object
#' @param cols  <[`tidy-select`][dplyr_tidy_select]> The variables you want 
#'   to get the frequencies for. 
#' @param group <[`tidy-select`][dplyr_tidy_select]> A selection of columns to 
#'   group the data by in addition to `treats`. This operates very similarly
#'   to `.by` from dplyr (for more info on that see [?dplyr_by][dplyr_by]). 
#'   See examples to see how it operates.
#' @param wt A character string. Add if you have a weighting variable and want
#'   to get weighted frequencies
#' @param drop_zero Logical. Determines if rows with 0 should be removed. 
#'   Default is `FALSE`.
#' @param decimals Number of decimals each number should be rounded to. Default
#'   is 1.
#' @param na.rm Logical. Determines if NAs should be kept or removed. Default is
#'   `TRUE`.
#' @param show_genpop Logical. Determines if there is a column showing the frequencies
#'   for the general population. Default is `FALSE` which does not include
#'   columns for the full sample. If `TRUE`, includeds two columns at
#'   the end for the full sample.
#' @param file_name A character string specifying the name of the file to be
#'   created with the frequencies and where the file will be located. File must
#'   end in .docx
#'
#' @export
get_all_freqs <- function(
  data, 
  cols, 
  group = NULL, 
  wt = NULL, 
  drop_zero = FALSE,
  decimals = 1,
  na.rm = TRUE,
  show_genpop = FALSE,
  file_name
) {

  # Prepare weights
  if (missing(wt)) {
    wt <- "wts"
    data[[wt]] <- rep(1, nrow(data))
  } else {
    wt <- rlang::as_name(rlang::ensym(wt))
    data[[wt]][is.na(data[[wt]])] <- 0
  }

  # Prepare group variables
  # if the data is grouped, get them using attr(data, "groups"), else set to NULL
  group_names <- if(inherits(data, "grouped_df")) setdiff(names(attr(data, "groups")), ".rows") else NULL
  # if group arg is missing set to NULL, else use as.character(substitute()) to capture it
  group_vars <- if (missing(group)) NULL else select_groups({{ group }}, data)
  # remove the "c" from the group_vars vector if it is there
  group_vars <- group_vars[group_vars != "c"]
  # combine group_names and group_vars for the final vector of group names
  # use unique to make sure there aren't any duplicates
  group_names <- unique(c(group_names, group_vars))

  
  # if cols is not missing, keep only the group_names and variables in cols
  if (!missing(cols)) data <- data %>% dplyr::select(tidyselect::all_of(group_names), {{ cols }}, wts)
  # ungroup the data so cols doesn't include the group variable
  data <- dplyr::ungroup(data)
  # get the columns you are iterating over
  cols <- get_col_names(data, {{ cols }})
  
  freqs <- purrr::map(
    cols,
    \(var) get_freqs(data, {{ var }}, group = {{ group_names }}, wt = {{ wt }}, drop_zero = drop_zero, decimals = decimals, na.rm = na.rm) %>% 
      prettytable(show_genpop = show_genpop)
  )

  my_doc <- officer::read_docx()

  # Print the tables
  purrr::walk(freqs, write_word_table, my_doc)
  print(my_doc, target = file_name) %>% invisible()

}
