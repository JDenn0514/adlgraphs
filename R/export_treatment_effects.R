#' Export Treatment Effects to Excel
#'
#' Iterates through a list of dependent variables, calculates treatment effects
#' using `tidysurvey::get_diffs`, and exports the results to a formatted Excel
#' workbook. It handles statistical testing, conditional formatting (color-
#' coding significance), and workbook management (appending to existing files).
#'
#' @param data A data frame containing the survey data.
#' @param dv_list A character vector of column names representing the dependent
#'   variables to analyze.
#' @param treats The column name (unquoted or string) identifying the treatment
#'   variable.
#' @param group Optional. The column name (unquoted or string) identifying a
#'   subgroup variable. Defaults to `NULL`.
#' @param pos_vars A character vector of dependent variable names where a
#'   positive difference is considered "good" (colored green).
#' @param neg_vars A character vector of dependent variable names where a
#'   positive difference is considered "bad" (colored red).
#' @param wt The column name of the weights variable. Defaults to "wts".
#' @param pval_adj Character string indicating the p-value adjustment method
#'   (e.g., "holm", "bonferroni", "fdr"). Passed to `stats::p.adjust`. Defaults
#'   to `NULL` (no adjustment).
#' @param conf_level Numeric value between 0 and 1 indicating the confidence
#'   level. Defaults to 0.95.
#' @param conf_method Character string indicating the method for confidence
#'   intervals ("wald" or "profile"). Defaults to "wald".
#' @param show_means Logical. Whether to include mean values in the output.
#'   Defaults to `TRUE`.
#' @param show_pct_change Logical. Whether to include percent change in the
#'   output. Defaults to `FALSE`.
#' @param decimals Integer. The number of decimal places for numeric outputs.
#'   Defaults to 3.
#' @param na.rm Logical. Whether to remove NA values during calculation.
#'   Defaults to `TRUE`.
#' @param filename String. The file path for the Excel workbook (e.g.,
#'   "output/results.xlsx").
#' @param sheet_name String. The name of the worksheet to create or append to.
#'   Defaults to "Treatment Effects".
#' @param append Logical. If `TRUE`, adds the sheet to an existing workbook if
#'   the file exists. If `FALSE`, overwrites the file. Defaults to `TRUE`.
#'
#' @return None. The function calculates statistics and writes them directly to
#'   an .xlsx file.
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' export_treatment_effects(
#'   data = survey_data,
#'   dv_list = c("satisfaction", "likelihood_to_recommend"),
#'   treats = treatment_group,
#'   group = segment,
#'   pos_vars = c("satisfaction"),
#'   filename = "results.xlsx",
#'   pval_adj = "holm"
#' )
#' }
export_treatment_effects <- function(
  data,
  dv_list,
  treats,
  group = NULL,
  pos_vars = NULL,
  neg_vars = NULL,
  wt = "wts",
  pval_adj = NULL,
  conf_level = 0.95,
  conf_method = c("wald", "profile"),
  show_means = TRUE,
  show_pct_change = FALSE,
  decimals = 3,
  na.rm = TRUE,
  filename,
  sheet_name = "Treatment Effects",
  append = TRUE
) {
  # 1. Initialize Workbook
  wb_setup <- init_workbook_logic(filename, sheet_name, append)
  wb <- wb_setup$wb
  sheet_name <- wb_setup$sheet_name

  current_row <- 1

  # 2. Loop through Dependent Variables
  for (i in seq_along(dv_list)) {
    dv <- dv_list[i]

    # A. Calculate Statistics and Prepare Data
    res <- calculate_and_format_results(
      data,
      {{ dv }},
      {{ treats }},
      {{ group }},
      {{ wt }},
      pval_adj,
      conf_level,
      conf_method,
      show_means,
      show_pct_change,
      decimals,
      na.rm
    )

    n_cols <- ncol(res$export_data)
    n_rows <- nrow(res$export_data)

    # B. Write Title
    dims_title <- openxlsx2::wb_dims(rows = current_row, cols = 1:n_cols)

    wb <- openxlsx2::wb_add_data(
      wb,
      sheet = sheet_name,
      x = res$var_label,
      dims = dims_title
    )

    wb <- openxlsx2::wb_merge_cells(wb, sheet = sheet_name, dims = dims_title)

    wb <- openxlsx2::wb_add_font(
      wb,
      sheet = sheet_name,
      dims = dims_title,
      bold = TRUE,
      size = 12
    )

    wb <- openxlsx2::wb_add_fill(
      wb,
      sheet = sheet_name,
      dims = dims_title,
      color = openxlsx2::wb_color(hex = "FFE6E6FA")
    )

    current_row <- current_row + 1

    # C. Write Data Table
    wb <- openxlsx2::wb_add_data(
      wb,
      sheet = sheet_name,
      x = res$export_data,
      start_col = 1,
      start_row = current_row,
      col_names = TRUE
    )

    # D. Apply Standard Styling
    wb <- style_general_table(
      wb,
      sheet_name,
      res$export_data,
      res$results_numeric,
      current_row,
      n_rows,
      n_cols,
      dv,
      pos_vars,
      neg_vars
    )

    # E. Apply Group Styling (Vertical Merging Only)
    if (length(res$group_indices) > 0) {
      wb <- style_grouping_columns(
        wb,
        sheet_name,
        res$export_data,
        current_row,
        n_rows,
        n_cols,
        res$group_indices
      )
    }

    current_row <- current_row + n_rows + 4
  }

  # 3. Finalize
  wb <- openxlsx2::wb_set_col_widths(
    wb,
    sheet = sheet_name,
    cols = 1:8,
    widths = "auto"
  )

  # Fix N column width buffer
  if (exists("res")) {
    col_n <- grep("^N$|^n$", colnames(res$export_data))
    if (length(col_n) > 0) {
      wb <- openxlsx2::wb_set_col_widths(
        wb,
        sheet = sheet_name,
        cols = col_n,
        widths = 10
      )
    }
  }

  # 4. Save
  openxlsx2::wb_save(wb, file = filename)

  cat("Exported to:", filename, "\n")
  cat("Sheet:", sheet_name, "\n")
  if (wb_setup$appended) cat("(Appended to existing file)\n")
}


# ------------------------------------------------------------------------------
# HELPER 1: WORKBOOK INITIALIZATION
# ------------------------------------------------------------------------------

init_workbook_logic <- function(filename, sheet_name, append) {
  appended <- FALSE

  if (append && file.exists(filename)) {
    message("Attempting to append to: ", filename)
    wb <- openxlsx2::wb_load(filename)
    appended <- TRUE

    # Handle duplicate sheet names
    existing_sheets <- openxlsx2::wb_get_sheet_names(wb)
    if (sheet_name %in% existing_sheets) {
      original_name <- sheet_name
      counter <- 1
      while (sheet_name %in% existing_sheets) {
        sheet_name <- paste0(original_name, " (", counter, ")")
        counter <- counter + 1
      }
    }
    wb <- openxlsx2::wb_add_worksheet(wb, sheet = sheet_name)
  } else {
    message("Creating new file: ", filename)
    wb <- openxlsx2::wb_workbook()
    wb <- openxlsx2::wb_add_worksheet(wb, sheet = sheet_name)
  }

  return(list(wb = wb, sheet_name = sheet_name, appended = appended))
}


# ------------------------------------------------------------------------------
# HELPER 2: DATA CALCULATION & FORMATTING
# ------------------------------------------------------------------------------
calculate_and_format_results <- function(
  data,
  dv,
  treats,
  group,
  wt,
  pval_adj,
  conf_level,
  conf_method,
  show_means,
  show_pct_change,
  decimals,
  na.rm
) {
  # 1. Run Stats
  if (is.null(group)) {
    results <- tidysurvey::get_diffs(
      data,
      x = {{ dv }},
      treats = {{ treats }},
      wt = wt,
      pval_adj = pval_adj,
      conf_level = conf_level,
      conf_method = conf_method,
      show_means = show_means,
      show_pct_change = show_pct_change,
      decimals = decimals,
      na.rm = na.rm
    )
    group_cols <- character(0)
  } else {
    results <- tidysurvey::get_diffs(
      data,
      x = {{ dv }},
      treats = {{ treats }},
      group = {{ group }},
      wt = wt,
      pval_adj = pval_adj,
      conf_level = conf_level,
      conf_method = conf_method,
      show_means = show_means,
      show_pct_change = show_pct_change,
      decimals = decimals,
      na.rm = na.rm
    )
    group_cols <- intersect(names(results), group)
  }

  # 2. Save numeric copy
  results_numeric <- results

  # 3. Format Strings
  # NOTE: Assuming make_percent is an internal helper in this package.
  # If it is from an external package (e.g., scales), use scales::percent()
  results$mean <- make_percent(results$mean)
  results$diffs <- make_percent(results$diffs)

  # 4. Get Labels
  var_label <- attr(results, "variable_label")
  if (is.null(var_label) || is.na(var_label)) {
    var_label <- paste("Results for", dv)
  }

  # 5. Rename Columns using Attributes
  export_data <- results
  current_names <- names(export_data)
  new_names <- current_names

  fallback_map <- c(
    "term" = "Treatment",
    "diffs" = "Difference in means relative to Control",
    "mean" = "Mean",
    "n" = "N",
    "conf.low" = "95% CI Lower",
    "conf.high" = "95% CI Upper",
    "p.value" = "P-value",
    "sig" = ""
  )

  for (j in seq_along(export_data)) {
    col_name <- current_names[j]
    lbl <- attr(results_numeric[[col_name]], "label")

    if (!is.null(lbl) && !is.na(lbl)) {
      new_names[j] <- lbl
    } else if (col_name %in% names(fallback_map)) {
      new_names[j] <- fallback_map[[col_name]]
    } else if (col_name %in% group_cols) {
      new_names[j] <- tools::toTitleCase(gsub("_", " ", col_name))
    }
  }

  if ("sig" %in% current_names) {
    new_names[which(current_names == "sig")] <- ""
  }

  colnames(export_data) <- new_names

  # Identify indices of grouping cols (Robust to renaming)
  group_indices <- which(current_names %in% group_cols)

  return(list(
    export_data = export_data,
    results_numeric = results_numeric,
    var_label = var_label,
    group_indices = group_indices
  ))
}


# ------------------------------------------------------------------------------
# HELPER 3: GENERAL STYLING (Fonts, Borders, Colors)
# ------------------------------------------------------------------------------
style_general_table <- function(
  wb,
  sheet_name,
  export_data,
  results_numeric,
  current_row,
  n_rows,
  n_cols,
  dv,
  pos_vars,
  neg_vars
) {
  # A. Header Styling
  dims_header <- openxlsx2::wb_dims(rows = current_row, cols = 1:n_cols)

  wb <- openxlsx2::wb_add_font(
    wb,
    sheet = sheet_name,
    dims = dims_header,
    bold = TRUE
  )

  wb <- openxlsx2::wb_add_border(
    wb,
    sheet = sheet_name,
    dims = dims_header,
    bottom_border = "thin",
    top_border = "thin",
    left_border = "thin",
    right_border = "thin"
  )

  wb <- openxlsx2::wb_add_cell_style(
    wb,
    sheet = sheet_name,
    dims = dims_header,
    wrap_text = TRUE,
    vertical = "center"
  )

  # B. Body Basic Borders
  dims_body <- openxlsx2::wb_dims(
    rows = (current_row + 1):(current_row + n_rows),
    cols = 1:n_cols
  )

  wb <- openxlsx2::wb_add_border(
    wb,
    sheet = sheet_name,
    dims = dims_body,
    bottom_border = "thin",
    top_border = "thin",
    left_border = "thin",
    right_border = "thin"
  )

  # C. Conditional Coloring
  wb <- add_significance_colors(
    wb,
    sheet_name,
    results_numeric,
    current_row,
    dv,
    pos_vars,
    neg_vars
  )

  # D. Number Formatting
  col_names <- colnames(export_data)

  # Find columns dynamically
  idx_decs <- grep("CI|P-value|p\\.value", col_names, ignore.case = TRUE)
  idx_n <- grep("^N$|^n$", col_names)

  # Align right: everything except groups, treatment text, and significance stars
  idx_text <- grep("Group|Treatment|sig|^$", col_names, ignore.case = TRUE)
  idx_align_right <- setdiff(1:n_cols, idx_text)

  if (length(idx_decs) > 0) {
    wb <- openxlsx2::wb_add_numfmt(
      wb,
      sheet = sheet_name,
      dims = openxlsx2::wb_dims(
        rows = (current_row + 1):(current_row + n_rows),
        cols = idx_decs
      ),
      numfmt = "0.000"
    )
  }

  if (length(idx_n) > 0) {
    wb <- openxlsx2::wb_add_numfmt(
      wb,
      sheet = sheet_name,
      dims = openxlsx2::wb_dims(
        rows = (current_row + 1):(current_row + n_rows),
        cols = idx_n
      ),
      numfmt = "#,##0"
    )
  }

  if (length(idx_align_right) > 0) {
    wb <- openxlsx2::wb_add_cell_style(
      wb,
      sheet = sheet_name,
      dims = openxlsx2::wb_dims(
        rows = (current_row + 1):(current_row + n_rows),
        cols = idx_align_right
      ),
      horizontal = "right"
    )
  }

  return(wb)
}


# ------------------------------------------------------------------------------
# HELPER 4: GROUP STYLING (Merges + Separators)
# ------------------------------------------------------------------------------
style_grouping_columns <- function(
  wb,
  sheet_name,
  export_data,
  current_row,
  n_rows,
  n_cols,
  group_indices
) {
  data_start_row <- current_row + 1

  # Merge and Align Group Columns
  for (g_idx in group_indices) {
    vec <- as.character(export_data[[g_idx]])
    rle_res <- rle(vec)

    curr_run_row <- data_start_row
    for (k in seq_along(rle_res$lengths)) {
      len <- rle_res$lengths[k]
      if (len > 1) {
        dims_merge <- openxlsx2::wb_dims(
          rows = curr_run_row:(curr_run_row + len - 1),
          cols = g_idx
        )
        wb <- openxlsx2::wb_merge_cells(
          wb,
          sheet = sheet_name,
          dims = dims_merge
        )
      }
      curr_run_row <- curr_run_row + len
    }

    # Vertically Center
    dims_group <- openxlsx2::wb_dims(
      rows = data_start_row:(data_start_row + n_rows - 1),
      cols = g_idx
    )
    wb <- openxlsx2::wb_add_cell_style(
      wb,
      sheet = sheet_name,
      dims = dims_group,
      vertical = "center"
    )
  }

  return(wb)
}


# ------------------------------------------------------------------------------
# HELPER 5: SIGNIFICANCE COLORS
# ------------------------------------------------------------------------------
add_significance_colors <- function(
  wb,
  sheet_name,
  data_numeric,
  start_row,
  dv_name,
  pos_vars,
  neg_vars
) {
  if (is.null(dv_name) || (!dv_name %in% pos_vars && !dv_name %in% neg_vars)) {
    return(wb)
  }

  # Find columns dynamically
  col_diff <- grep(
    "diffs|estimate|difference",
    names(data_numeric),
    ignore.case = TRUE
  )[1]
  col_pval <- grep(
    "p.value|p_value|p-value",
    names(data_numeric),
    ignore.case = TRUE
  )[1]

  if (is.na(col_diff) || is.na(col_pval)) {
    return(wb)
  }

  # Extract vectors
  diff_vals <- data_numeric[[col_diff]]
  p_vals <- data_numeric[[col_pval]]

  green_rows <- integer(0)
  red_rows <- integer(0)

  # Logic
  if (dv_name %in% pos_vars) {
    green_idx <- which(diff_vals > 0 & p_vals < 0.05)
    red_idx <- which(diff_vals < 0 & p_vals < 0.05)
  } else if (dv_name %in% neg_vars) {
    green_idx <- which(diff_vals < 0 & p_vals < 0.05)
    red_idx <- which(diff_vals > 0 & p_vals < 0.05)
  }

  # Map to Excel rows
  if (length(green_idx) > 0) {
    green_rows <- start_row + green_idx
  }
  if (length(red_idx) > 0) {
    red_rows <- start_row + red_idx
  }

  total_cols <- ncol(data_numeric)

  if (length(green_rows) > 0) {
    dims_green <- openxlsx2::wb_dims(rows = green_rows, cols = 1:total_cols)

    wb <- openxlsx2::wb_add_fill(
      wb,
      sheet = sheet_name,
      dims = dims_green,
      color = openxlsx2::wb_color(hex = "FFC6EFCE")
    )

    wb <- openxlsx2::wb_add_font(
      wb,
      sheet = sheet_name,
      dims = dims_green,
      color = openxlsx2::wb_color(hex = "FF006100")
    )
  }

  if (length(red_rows) > 0) {
    dims_red <- openxlsx2::wb_dims(rows = red_rows, cols = 1:total_cols)

    wb <- openxlsx2::wb_add_fill(
      wb,
      sheet = sheet_name,
      dims = dims_red,
      color = openxlsx2::wb_color(hex = "FFFFC7CE")
    )

    wb <- openxlsx2::wb_add_font(
      wb,
      sheet = sheet_name,
      dims = dims_red,
      color = openxlsx2::wb_color(hex = "FF9C0006")
    )
  }

  return(wb)
}
