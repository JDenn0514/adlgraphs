# ============================================================================
# Helper Functions
# ============================================================================

#' Calculate design effect (DEFF) for weighted data
#' @param weights Vector of weights
#' @return Design effect value
calc_design_effect <- function(weights) {
  # Remove NAs
  weights <- weights[!is.na(weights)]

  if (length(weights) == 0) {
    return(1)
  }

  # DEFF = n * sum(w^2) / (sum(w))^2
  n <- length(weights)
  sum_w <- sum(weights)
  sum_w_sq <- sum(weights^2)

  deff <- (n * sum_w_sq) / (sum_w^2)

  return(deff)
}

#' Calculate margin of error for weighted proportion
#' @param p Proportion (as percentage, e.g., 30 for 30%)
#' @param n Unweighted sample size
#' @param deff Design effect
#' @return Margin of error (as decimal, e.g., 0.0435 for 4.35 percentage points)
calc_moe <- function(p, n, deff) {
  # Convert percentage to proportion
  p_prop <- p / 100

  # Handle edge cases
  if (is.na(p) || is.na(n) || is.na(deff) || n == 0) {
    return(NA_real_)
  }

  # Avoid issues with 0% or 100%
  if (p_prop <= 0 || p_prop >= 1) {
    return(0)
  }

  # Standard error accounting for design effect
  # SE = sqrt(DEFF * p * (1-p) / n)
  se <- sqrt(deff * p_prop * (1 - p_prop) / n)

  # Margin of error at 95% confidence (1.96 * SE)
  # Return as decimal (not percentage points)
  moe <- 1.96 * se

  return(moe)
}


# ============================================================================
# Main Function
# ============================================================================

#' Create Polling Crosstabs and Export to Excel
#'
#' This function takes survey data and creates formatted crosstabs showing
#' weighted percentages of row variables across different subgroups. The results
#' are exported to an Excel file with proper formatting.
#'
#' Weighted percentages are calculated via \code{get_freqs()}, which supports
#' plain data frames with a weight column as well as \code{survey.design} and
#' \code{svyrep.design} objects.
#'
#' @param data A data frame or survey design object containing survey responses
#' @param row_vars Character vector of variable names to display as rows
#' @param subgroup_vars Character vector of variable names to use as column subgroups
#' @param wt_var Character string specifying the name of the weight variable.
#'   Ignored when \code{data} is a \code{survey.design} or \code{svyrep.design}
#'   object, in which case weights are taken from the design.
#' @param min_n Integer specifying minimum unweighted sample size for a subgroup
#'   to be included (default: 75)
#' @param file_name Character string specifying the Excel file path/name
#' @param sheet_name Character string specifying the sheet name
#' @param overwrite_existing_sheet Logical indicating whether to overwrite existing
#'   sheet with same name (default: TRUE)
#' @param summary_string Character string to display in the top-left merged cell
#'   (default: "")
#'
#' @return Data frame with columns: subgroup_var, subgroup_var_label, subgroup_val,
#'   n, row_var, row_var_label, row_val, pct (as decimal), moe (as decimal).
#'   All categorical columns are factors with levels in the order passed to the function.
#'   Sorted by subgroup variables then row variables.
#'
#' @examples
#' \dontrun{
#' results <- create_polling_crosstabs(
#'   data = survey_data,
#'   row_vars = c("job_approval", "direction"),
#'   subgroup_vars = c("gender", "race"),
#'   wt_var = "weight",
#'   min_n = 75,
#'   file_name = "crosstabs.xlsx",
#'   sheet_name = "Results",
#'   summary_string = "January 2026 National Poll"
#' )
#' }
#'
#' @export
create_polling_crosstabs <- function(
  data,
  row_vars,
  subgroup_vars,
  wt_var,
  min_n = 75,
  file_name,
  sheet_name,
  overwrite_existing_sheet = TRUE,
  summary_string = ""
) {
  # ============================================================================
  # Load Required Packages
  # ============================================================================

  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop(
      "Package 'openxlsx2' is required. Please install it with: install.packages('openxlsx2')"
    )
  }

  # ============================================================================
  # Input Validation
  # ============================================================================

  # Determine whether data is a survey design object; if so, extract the
  # underlying data frame for subsetting and label lookups
  is_survey <- inherits(data, c("survey.design", "svyrep.design", "tbl_svy"))
  data_df <- if (is_survey) data$variables else as.data.frame(data)

  # Check that all specified variables exist in the data
  all_vars <- c(row_vars, subgroup_vars)

  # Only validate wt_var for plain data frames; survey designs carry their own weights
  if (!is_survey) {
    all_vars <- c(all_vars, wt_var)
  }

  missing_vars <- base::setdiff(all_vars, base::names(data_df))
  if (base::length(missing_vars) > 0) {
    stop(
      "The following variables are not found in the data: ",
      base::paste(missing_vars, collapse = ", ")
    )
  }

  # For plain data frames, check that weights are numeric
  if (!is_survey && !base::is.numeric(data_df[[wt_var]])) {
    stop("Weight variable '", wt_var, "' must be numeric")
  }

  # ============================================================================
  # Build Column Structure (Subgroups)
  # ============================================================================

  columns_list <- base::list()

  # Add Overall column first
  columns_list[[1]] <- base::list(
    var_name = "Overall",
    var_label = "Overall",
    value = "Overall",
    data_subset = data,
    unweighted_n = base::nrow(data_df),
    deff = calc_design_effect(
      if (is_survey) data$variables[[wt_var]] else data_df[[wt_var]]
    )
  )

  # Add each subgroup variable's values as separate columns
  for (var in subgroup_vars) {
    # Get the variable label as a single scalar string, falling back to the
    # variable name. force_scalar() guards against labelled vectors returning
    # a named vector of length > 1 from attr_var_label()
    raw_label <- attr_var_label(data_df[[var]], if_null = "name")
    var_label <- if (base::length(raw_label) > 1) {
      var
    } else {
      base::as.character(raw_label[[1]])
    }

    unique_values <- base::sort(base::unique(data_df[[var]][
      !base::is.na(data_df[[var]])
    ]))

    for (val in unique_values) {
      row_idx <- !base::is.na(data_df[[var]]) & data_df[[var]] == val
      unweighted_n <- base::sum(row_idx)

      if (unweighted_n >= min_n) {
        data_subset <- if (is_survey) data[row_idx, ] else data_df[row_idx, ]

        wt_vec <- if (is_survey) {
          data_subset$variables[[wt_var]]
        } else {
          data_subset[[wt_var]]
        }

        columns_list[[base::length(columns_list) + 1]] <- base::list(
          var_name = var,
          var_label = var_label,
          value = val,
          data_subset = data_subset,
          unweighted_n = unweighted_n,
          deff = calc_design_effect(wt_vec)
        )
      }
    }
  }

  num_cols <- base::length(columns_list)

  # ============================================================================
  # Build Row Structure (Questions)
  # ============================================================================

  rows_list <- base::list()

  for (var in row_vars) {
    # Same scalar-coercion guard as above
    raw_label <- attr_var_label(data_df[[var]], if_null = "name")
    var_label <- if (base::length(raw_label) > 1) {
      var
    } else {
      base::as.character(raw_label[[1]])
    }

    unique_values <- base::unique(data_df[[var]][!base::is.na(data_df[[var]])])

    if (base::is.factor(data_df[[var]])) {
      unique_values <- base::levels(data_df[[var]])
      unique_values <- unique_values[unique_values %in% data_df[[var]]]
    }

    for (val in unique_values) {
      rows_list[[base::length(rows_list) + 1]] <- base::list(
        var_name = var,
        var_label = var_label,
        value = val
      )
    }
  }

  num_rows <- base::length(rows_list)

  # ============================================================================
  # Calculate Weighted Percentages via get_freqs()
  # ============================================================================

  # Create matrix to hold percentages (0-100 scale for Excel; converted to
  # decimal 0-1 in the return data frame and for the percentage cell format)
  crosstab_matrix <- base::matrix(NA, nrow = num_rows, ncol = num_cols)

  for (j in 1:num_cols) {
    col_data <- columns_list[[j]]$data_subset

    for (var in base::unique(base::sapply(rows_list, `[[`, "var_name"))) {
      # Call get_freqs() for this row variable within this subgroup.
      # For plain data frames, pass wt_var via the wt argument.
      # For survey designs, weights are taken from the design automatically.
      freqs <- if (is_survey) {
        get_freqs(
          data = col_data,
          x = tidyselect::all_of(var),
          na.rm = TRUE
        )
      } else {
        get_freqs(
          data = col_data,
          x = tidyselect::all_of(var),
          wt = !!rlang::sym(wt_var),
          na.rm = TRUE
        )
      }

      # get_freqs() returns a tibble with columns [var], n, pct
      # pct is already a decimal proportion (0-1); convert to 0-100 for the matrix
      for (i in 1:num_rows) {
        if (rows_list[[i]]$var_name != var) {
          next
        }

        row_val <- base::as.character(rows_list[[i]]$value)
        freq_row <- freqs[base::as.character(freqs[[var]]) == row_val, ]

        crosstab_matrix[i, j] <- if (base::nrow(freq_row) > 0) {
          freq_row$pct[[1]] * 100 # store as 0-100 for downstream Excel use
        } else {
          0
        }
      }
    }
  }

  # ============================================================================
  # Build Factor Levels for Ordering
  # ============================================================================

  # Helper to safely extract a single scalar label from a potentially
  # multi-length attr_var_label() result
  safe_label <- function(var) {
    raw <- attr_var_label(data_df[[var]], if_null = "name")
    if (base::length(raw) > 1) var else base::as.character(raw[[1]])
  }

  subgroup_var_levels <- c("Overall", subgroup_vars)

  subgroup_var_label_levels <- base::unique(c(
    "Overall",
    base::vapply(subgroup_vars, safe_label, character(1))
  ))

  subgroup_val_levels <- base::unique(
    base::vapply(
      columns_list,
      function(col) base::as.character(col$value),
      character(1)
    )
  )

  row_var_levels <- row_vars

  row_var_label_levels <- base::unique(
    base::vapply(row_vars, safe_label, character(1))
  )

  row_val_levels <- base::unique(
    base::vapply(
      rows_list,
      function(r) base::as.character(r$value),
      character(1)
    )
  )

  # ============================================================================
  # Build Return Data Frame
  # ============================================================================

  # Create long-format data frame with one row per cell in the crosstab matrix.
  # Subgroup columns come first, followed by row variable columns.
  results_list <- base::list()

  for (j in 1:num_cols) {
    for (i in 1:num_rows) {
      pct_percentage <- crosstab_matrix[i, j] # 0-100 scale
      pct_decimal <- pct_percentage / 100 # 0-1 scale for return df
      n <- columns_list[[j]]$unweighted_n
      deff <- columns_list[[j]]$deff
      moe <- calc_moe(pct_percentage, n, deff)

      results_list[[base::length(results_list) + 1]] <- base::data.frame(
        subgroup_var = columns_list[[j]]$var_name,
        subgroup_var_label = columns_list[[j]]$var_label,
        subgroup_val = base::as.character(columns_list[[j]]$value),
        n = n,
        row_var = rows_list[[i]]$var_name,
        row_var_label = rows_list[[i]]$var_label,
        row_val = base::as.character(rows_list[[i]]$value),
        pct = pct_decimal,
        moe = moe,
        stringsAsFactors = FALSE
      )
    }
  }

  results_df <- base::do.call(base::rbind, results_list)

  # Convert to factors with levels in the order they were passed
  results_df$subgroup_var <- base::factor(
    results_df$subgroup_var,
    levels = subgroup_var_levels
  )
  results_df$subgroup_var_label <- base::factor(
    results_df$subgroup_var_label,
    levels = subgroup_var_label_levels
  )
  results_df$subgroup_val <- base::factor(
    results_df$subgroup_val,
    levels = subgroup_val_levels
  )
  results_df$row_var <- base::factor(
    results_df$row_var,
    levels = row_var_levels
  )
  results_df$row_var_label <- base::factor(
    results_df$row_var_label,
    levels = row_var_label_levels
  )
  results_df$row_val <- base::factor(
    results_df$row_val,
    levels = row_val_levels
  )

  # Sort by subgroup variables first, then row variables.
  # Factor ordering ensures the sort respects the original input order.
  results_df <- results_df[
    base::order(
      results_df$subgroup_var,
      results_df$subgroup_val,
      results_df$row_var,
      results_df$row_val
    ),
  ]

  base::rownames(results_df) <- NULL

  # ============================================================================
  # Build Excel Structure
  # ============================================================================

  # Create header rows:
  #   Row 1 - Subgroup variable labels (merged across their value columns)
  #   Row 2 - Subgroup values
  #   Row 3 - Unweighted n-sizes

  # summary_string appears in the top-left merged cell (a1:b2)
  header_row1 <- c(summary_string, "", base::rep(NA, num_cols))
  header_row2 <- c("", "", base::rep(NA, num_cols))
  header_row3 <- c("", "n (unweighted)", base::rep(NA, num_cols))

  for (j in 1:num_cols) {
    header_row1[j + 2] <- columns_list[[j]]$var_label
    header_row2[j + 2] <- base::as.character(columns_list[[j]]$value)
    header_row3[j + 2] <- columns_list[[j]]$unweighted_n
  }

  # Build data rows:
  #   Columns 1-2  - text index columns (variable label, response value)
  #   Columns 3+   - numeric percentage values (decimal, formatted as % in Excel)
  data_rows_text <- base::matrix(NA, nrow = num_rows, ncol = 2)
  data_rows_numeric <- base::matrix(NA, nrow = num_rows, ncol = num_cols)

  for (i in 1:num_rows) {
    data_rows_text[i, 1] <- rows_list[[i]]$var_label
    data_rows_text[i, 2] <- base::as.character(rows_list[[i]]$value)
    # Divide by 100: Excel's "0%" format expects a decimal (0.30 -> "30%")
    data_rows_numeric[i, ] <- crosstab_matrix[i, ] / 100
  }

  # ============================================================================
  # Create Excel Workbook
  # ============================================================================

  if (base::file.exists(file_name)) {
    wb <- openxlsx2::wb_load(file_name)

    if (sheet_name %in% openxlsx2::wb_get_sheet_names(wb)) {
      if (overwrite_existing_sheet) {
        wb <- openxlsx2::wb_remove_worksheet(wb, sheet = sheet_name)
        wb <- openxlsx2::wb_add_worksheet(wb, sheet = sheet_name)
      } else {
        stop(
          "Sheet '",
          sheet_name,
          "' already exists in '",
          file_name,
          "'. Set overwrite_existing_sheet = TRUE to overwrite."
        )
      }
    } else {
      wb <- openxlsx2::wb_add_worksheet(wb, sheet = sheet_name)
    }
  } else {
    wb <- openxlsx2::wb_workbook()
    wb <- openxlsx2::wb_add_worksheet(wb, sheet = sheet_name)
  }

  # ============================================================================
  # BATCH WRITE: Separate calls preserve column types
  # ============================================================================

  # Header rows (text)
  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet_name,
    x = base::t(header_row1),
    start_row = 1,
    start_col = 1,
    col_names = FALSE
  )
  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet_name,
    x = base::t(header_row2),
    start_row = 2,
    start_col = 1,
    col_names = FALSE
  )
  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet_name,
    x = base::t(header_row3),
    start_row = 3,
    start_col = 1,
    col_names = FALSE
  )

  # Index columns (text)
  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet_name,
    x = data_rows_text,
    start_row = 4,
    start_col = 1,
    col_names = FALSE
  )

  # Data cells (numeric — must stay numeric so the percentage format applies)
  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = sheet_name,
    x = data_rows_numeric,
    start_row = 4,
    start_col = 3,
    col_names = FALSE
  )

  # ============================================================================
  # Apply Cell Styling
  # ============================================================================

  # In openxlsx2, fill/font/numfmt are applied directly via wb_add_fill(),
  # wb_add_font(), and wb_add_numfmt() rather than through create_cell_style().

  gray <- openxlsx2::wb_color(hex = "FFD3D3D3")
  white <- openxlsx2::wb_color(hex = "FFFFFFFF")
  black <- openxlsx2::wb_color(hex = "FF000000")

  # ---- Gray fill + bold + center align: top 2 header rows, all columns ----
  wb <- openxlsx2::wb_add_fill(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 1:2, cols = 1:(num_cols + 2)),
    color = gray
  )
  wb <- openxlsx2::wb_add_font(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 1:2, cols = 1:(num_cols + 2)),
    bold = TRUE
  )
  wb <- openxlsx2::wb_add_cell_style(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 1:2, cols = 1:(num_cols + 2)),
    horizontal = "center",
    vertical = "center",
    wrap_text = TRUE
  )

  # ---- Gray fill + bold + center align: n-row index columns (cols 1-2) ----
  wb <- openxlsx2::wb_add_fill(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 3, cols = 1:2),
    color = gray
  )
  wb <- openxlsx2::wb_add_font(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 3, cols = 1:2),
    bold = TRUE
  )
  wb <- openxlsx2::wb_add_cell_style(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 3, cols = 1:2),
    horizontal = "center",
    vertical = "center",
    wrap_text = TRUE
  )

  # ---- White fill + center align: n-row data columns ----
  wb <- openxlsx2::wb_add_fill(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 3, cols = 3:(num_cols + 2)),
    color = white
  )
  wb <- openxlsx2::wb_add_cell_style(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 3, cols = 3:(num_cols + 2)),
    horizontal = "center",
    vertical = "center"
  )

  # ---- Gray fill + bold + center align: index columns for all data rows ----
  wb <- openxlsx2::wb_add_fill(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 4:(num_rows + 3), cols = 1:2),
    color = gray
  )
  wb <- openxlsx2::wb_add_font(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 4:(num_rows + 3), cols = 1:2),
    bold = TRUE
  )
  wb <- openxlsx2::wb_add_cell_style(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 4:(num_rows + 3), cols = 1:2),
    horizontal = "center",
    vertical = "center",
    wrap_text = TRUE
  )

  # ---- White fill + percentage format + center align: data cells ----
  wb <- openxlsx2::wb_add_fill(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 4:(num_rows + 3), cols = 3:(num_cols + 2)),
    color = white
  )
  wb <- openxlsx2::wb_add_numfmt(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 4:(num_rows + 3), cols = 3:(num_cols + 2)),
    numfmt = "0%"
  )
  wb <- openxlsx2::wb_add_cell_style(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 4:(num_rows + 3), cols = 3:(num_cols + 2)),
    horizontal = "center",
    vertical = "center"
  )

  # ============================================================================
  # Add Borders Between Variable Groups
  # ============================================================================

  # IMPORTANT: update = TRUE merges the new border side onto any existing
  # border styles on those cells, rather than overwriting all sides.
  # Without this, applying a top border would clear any left/right borders
  # that were already applied to the same cells.

  black <- openxlsx2::wb_color(hex = "FF000000")

  # Horizontal borders between row variables
  current_var <- rows_list[[1]]$var_name
  current_label <- rows_list[[1]]$var_label

  for (i in 2:num_rows) {
    if (rows_list[[i]]$var_name != current_var) {
      excel_row <- i + 3
      border_style <- if (rows_list[[i]]$var_label == current_label) {
        "dashed"
      } else {
        "medium"
      }

      wb <- openxlsx2::wb_add_border(
        wb,
        sheet = sheet_name,
        dims = openxlsx2::wb_dims(rows = excel_row, cols = 1:(num_cols + 2)),
        top_border = border_style,
        top_color = black,
        bottom_border = NULL,
        left_border = NULL,
        right_border = NULL,
        update = TRUE
      )

      current_var <- rows_list[[i]]$var_name
      current_label <- rows_list[[i]]$var_label
    }
  }

  # Vertical borders between subgroup variables
  current_var <- columns_list[[1]]$var_name
  current_label <- columns_list[[1]]$var_label

  for (j in 2:num_cols) {
    if (columns_list[[j]]$var_name != current_var) {
      excel_col <- j + 2
      border_style <- if (columns_list[[j]]$var_label == current_label) {
        "dashed"
      } else {
        "medium"
      }

      wb <- openxlsx2::wb_add_border(
        wb,
        sheet = sheet_name,
        dims = openxlsx2::wb_dims(rows = 1:(num_rows + 3), cols = excel_col),
        left_border = border_style,
        left_color = black,
        top_border = NULL,
        bottom_border = NULL,
        right_border = NULL,
        update = TRUE
      )

      current_var <- columns_list[[j]]$var_name
      current_label <- columns_list[[j]]$var_label
    }
  }

  # Right border after index columns (col 2), all rows
  wb <- openxlsx2::wb_add_border(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 1:(num_rows + 3), cols = 2),
    right_border = "medium",
    right_color = black,
    top_border = NULL,
    bottom_border = NULL,
    left_border = NULL,
    update = TRUE
  )

  # Bottom border after header rows (row 2), all columns
  wb <- openxlsx2::wb_add_border(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 2, cols = 1:(num_cols + 2)),
    bottom_border = "medium",
    bottom_color = black,
    top_border = NULL,
    left_border = NULL,
    right_border = NULL,
    update = TRUE
  )

  # Bottom border after n-sizes row (row 3), all columns
  wb <- openxlsx2::wb_add_border(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 3, cols = 1:(num_cols + 2)),
    bottom_border = "medium",
    bottom_color = black,
    top_border = NULL,
    left_border = NULL,
    right_border = NULL,
    update = TRUE
  )

  # ============================================================================
  # Merge Cells
  # ============================================================================

  # Top-left merged cell (a1:b2) — displays summary_string
  wb <- openxlsx2::wb_merge_cells(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 1:2, cols = 1:2)
  )

  # Merge subgroup variable label cells in header row 1
  # Consecutive columns sharing the same label are merged into one cell
  current_label <- header_row1[3]
  start_col <- 3

  for (j in 3:(num_cols + 2)) {
    if (j == (num_cols + 2) || header_row1[j + 1] != current_label) {
      if (j > start_col) {
        wb <- openxlsx2::wb_merge_cells(
          wb,
          sheet = sheet_name,
          dims = openxlsx2::wb_dims(rows = 1, cols = start_col:j)
        )
      }
      if (j < (num_cols + 2)) {
        current_label <- header_row1[j + 1]
        start_col <- j + 1
      }
    }
  }

  # Merge row variable label cells in index column 1
  # Consecutive rows sharing the same label are merged into one cell
  current_label <- data_rows_text[1, 1]
  start_row <- 4

  for (i in 1:num_rows) {
    excel_row <- i + 3
    if (i == num_rows || data_rows_text[i + 1, 1] != current_label) {
      if (excel_row > start_row) {
        wb <- openxlsx2::wb_merge_cells(
          wb,
          sheet = sheet_name,
          dims = openxlsx2::wb_dims(rows = start_row:excel_row, cols = 1)
        )
      }
      if (i < num_rows) {
        current_label <- data_rows_text[i + 1, 1]
        start_row <- excel_row + 1
      }
    }
  }

  # ============================================================================
  # Set Column Widths
  # ============================================================================

  wb <- openxlsx2::wb_set_col_widths(
    wb,
    sheet = sheet_name,
    cols = 1,
    widths = 50
  )
  wb <- openxlsx2::wb_set_col_widths(
    wb,
    sheet = sheet_name,
    cols = 2,
    widths = 40
  )
  wb <- openxlsx2::wb_set_col_widths(
    wb,
    sheet = sheet_name,
    cols = 3:(num_cols + 2),
    widths = 20
  )

  # ============================================================================
  # Freeze Panes
  # ============================================================================

  # Freeze top 2 rows and left 2 columns (first active cell = C3)
  wb <- openxlsx2::wb_freeze_pane(
    wb,
    sheet = sheet_name,
    first_active_row = 3,
    first_active_col = 3
  )

  # ============================================================================
  # Save Workbook
  # ============================================================================

  openxlsx2::wb_save(wb, file = file_name, overwrite = TRUE)

  base::message(
    "Crosstabs successfully exported to: ",
    file_name,
    " (Sheet: ",
    sheet_name,
    ")"
  )

  return(results_df)
}

# ============================================================================
# Example Usage
# ============================================================================

#' @examples
#' \dontrun{
#' set.seed(123)
#' n <- 500
#' survey_data <- data.frame(
#'   gender       = sample(c("Male", "Female"), n, replace = TRUE),
#'   race         = sample(c("White", "Black", "Hispanic", "Other"), n,
#'                         replace = TRUE, prob = c(0.6, 0.15, 0.15, 0.1)),
#'   job_approval = sample(c("Strongly approve", "Somewhat approve",
#'                           "Somewhat disapprove", "Strongly disapprove"),
#'                         n, replace = TRUE),
#'   right_direction = sample(c("Right direction", "Wrong track"),
#'                            n, replace = TRUE),
#'   weight = runif(n, 0.5, 2.0)
#' )
#'
#' attr(survey_data$gender,          "label") <- "Gender"
#' attr(survey_data$race,            "label") <- "Race/Ethnicity"
#' attr(survey_data$job_approval,    "label") <- "Job Approval"
#' attr(survey_data$right_direction, "label") <- "Country Direction"
#'
#' results <- create_polling_crosstabs(
#'   data                   = survey_data,
#'   row_vars               = c("job_approval", "right_direction"),
#'   subgroup_vars          = c("gender", "race"),
#'   wt_var                 = "weight",
#'   min_n                  = 75,
#'   file_name              = "polling_crosstabs.xlsx",
#'   sheet_name             = "January 2026",
#'   overwrite_existing_sheet = TRUE,
#'   summary_string         = "National Poll - January 2026"
#' )
#'
#' head(results)
#' levels(results$subgroup_var)  # "Overall", "gender", "race"
#' levels(results$row_var)       # "job_approval", "right_direction"
#' }
