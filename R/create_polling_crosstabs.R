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

#' Vectorized margin of error
#'
#' Elementwise equivalent of [calc_moe()], used to build the returned long
#' data frame in one pass. `calc_moe()` branches on `if (is.na(p) || ...)`, so
#' it only takes scalars and has to be called once per crosstab cell -- tens of
#' thousands of times on a large sheet.
#'
#' The two must agree exactly. The precedence below matches `calc_moe()`:
#' missing inputs or `n == 0` give `NA`, and that wins over the 0%/100% rule,
#' which gives 0.
#'
#' @param p Vector of proportions (as percentages, e.g. 30 for 30%)
#' @param n Vector of unweighted sample sizes
#' @param deff Vector of design effects
#' @return Vector of margins of error (as decimals)
#' @noRd
calc_moe_vec <- function(p, n, deff) {
  p_prop <- p / 100

  moe <- 1.96 * base::sqrt(deff * p_prop * (1 - p_prop) / n)

  # 0% and 100% carry no sampling error under this formula
  at_bound <- !base::is.na(p_prop) & (p_prop <= 0 | p_prop >= 1)
  moe[at_bound] <- 0

  # Missing inputs or an empty base are not estimable, and take precedence
  not_estimable <- base::is.na(p) |
    base::is.na(n) |
    base::is.na(deff) |
    (!base::is.na(n) & n == 0)
  moe[not_estimable] <- NA_real_

  moe
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
#' @param na_rm Logical; whether to remove rows with missing values in x and group before computing frequencies. Default is TRUE.
#' @param file_name Character string specifying the Excel file path/name
#' @param sheet_name Character string specifying the sheet name
#' @param overwrite_existing_sheet Logical indicating whether to overwrite existing
#'   sheet with same name (default: TRUE)
#' @param summary_string Character string to display in the top-left merged cell
#'   (default: "")
#' @param wb An \code{openxlsx2} \code{wbWorkbook} to add this sheet to, as
#'   returned by \code{attr(<result>, "wb")} from a previous call. When
#'   supplied, \code{file_name} is not read from disk. Use this together with
#'   \code{save = FALSE} to build a multi-sheet workbook in memory; see Details.
#' @param save Logical indicating whether to write the workbook to
#'   \code{file_name} (default: TRUE). Set to \code{FALSE} for every call but
#'   the last when building a multi-sheet workbook.
#'
#' @details
#' \strong{Per-question unweighted bases.} Each question is preceded by an
#' \code{n (unweighted)} row giving its base in every column. The base in the
#' sheet header covers the whole column; a question asked of only part of the
#' sample has a smaller one, and a question nobody in a column was asked
#' reads \code{0}. Reading the two together says which subset of the sample
#' answered a given question -- and explains the blank cells described below,
#' which sit under a base of \code{0}.
#'
#' The row is written once per question, and suppressed where it would only
#' repeat the row above. An item's collapse carries the same base as the item
#' it collapses, so a collapse that directly follows its parent does not
#' repeat it. That is recognised by an identical variable label on the row
#' variable above \emph{and} an identical base in every column, so a shared
#' label never hides a base that genuinely differs.
#'
#' These rows appear in the Excel output only. The returned data frame is
#' unchanged, and still carries one row per response value per column.
#'
#' \strong{Items with no base in a subgroup.} A question asked of only some
#' respondents can have no base at all inside a given column -- for example a
#' follow-up asked only of one candidate's voters, crossed against a vote-choice
#' banner, is empty in every other candidate's column. Those cells are left
#' blank in the Excel output and carry \code{NA} for \code{pct} and \code{moe}
#' in the returned data frame. Blank therefore means "nobody in this column was
#' asked this", which is different from \code{0}, which means "asked, and nobody
#' gave this answer".
#'
#' This applies under the default \code{na_rm = TRUE}. With \code{na_rm = FALSE},
#' missingness is reported as its own category instead.
#'
#' \strong{Building a multi-sheet workbook.} By default each call reads
#' \code{file_name} back off disk with \code{openxlsx2::wb_load()}, adds its
#' sheet, and writes the whole file out again. That round trip re-parses every
#' style record already in the file, so it gets steadily more expensive as
#' sheets accumulate -- measured at roughly 9x slower appending one sheet to a
#' five-sheet workbook than writing the same sheet to a fresh file. The
#' statistics are unaffected; the cost is all Excel serialization.
#'
#' Pass the workbook along instead, and save once at the end:
#'
#' \preformatted{
#' wb <- NULL
#' for (s in sheets) {
#'   res <- create_polling_crosstabs(
#'     data = d, row_vars = s$rows, subgroup_vars = s$cols,
#'     wt_var = "wts", file_name = "out.xlsx", sheet_name = s$name,
#'     wb = wb, save = FALSE
#'   )
#'   wb <- attr(res, "wb")
#' }
#' openxlsx2::wb_save(wb, "out.xlsx", overwrite = TRUE)
#' }
#'
#' @return Data frame with columns: subgroup_var, subgroup_var_label, subgroup_val,
#'   n, row_var, row_var_label, row_val, pct (as decimal), moe (as decimal).
#'   All categorical columns are factors with levels in the order passed to the function.
#'   Sorted by subgroup variables then row variables. The workbook the sheet was
#'   written into is attached as the \code{"wb"} attribute, for passing back in
#'   via the \code{wb} argument.
#'
#' @examples
#' \dontrun{
#' results <- create_polling_crosstabs(
#'   data = survey_data,
#'   row_vars = c("job_approval", "direction"),
#'   subgroup_vars = c("gender", "race"),
#'   wt_var = "weight",
#'   min_n = 75,
#'   na_rm = TRUE,
#'   file_name = "crosstabs.xlsx",
#'   sheet_name = "Results",
#'   summary_string = "January 2026 National Poll",
#' )
#' }
#'
#' @export
create_polling_crosstabs <- function(
  data,
  row_vars,
  subgroup_vars,
  wt_var,
  na_rm = TRUE,
  min_n = 75,
  file_name,
  sheet_name,
  overwrite_existing_sheet = TRUE,
  summary_string = "",
  wb = NULL,
  save = TRUE
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

  # Hoisted out of the column loop: the row variable names do not change, and
  # rebuilding this per column walks the whole rows_list every time.
  row_var_names <- base::unique(base::vapply(
    rows_list,
    function(r) r$var_name,
    character(1)
  ))

  # Unweighted base for every question in every column, so a reader can see
  # which subset of the sample each question was actually asked of. The column
  # header's n covers the whole column; a question asked of a filtered set has
  # a smaller one, and a question nobody in that column was asked has none.
  question_n <- base::matrix(
    0L,
    nrow = base::length(row_var_names),
    ncol = num_cols,
    dimnames = base::list(row_var_names, NULL)
  )

  for (j in 1:num_cols) {
    col_data <- columns_list[[j]]$data_subset
    col_data_vars <- if (is_survey) col_data$variables else col_data

    # Filled in its own pass, before the structural-empty skip below, so a
    # question nobody in this column was asked still records its 0 rather than
    # being skipped over. Under na_rm = FALSE missing answers are a reported
    # category, so they count toward the base.
    for (var in row_var_names) {
      question_n[var, j] <- if (na_rm) {
        base::sum(!base::is.na(col_data_vars[[var]]))
      } else {
        base::length(col_data_vars[[var]])
      }
    }

    for (var in row_var_names) {
      # An item can have no base at all inside a subgroup -- a question asked
      # only of a filtered set of respondents, crossed against a column that
      # excludes all of them. get_freqs() treats that as an error ("After
      # removing NAs, no rows remain"), which would abort the whole sheet over
      # cells that are legitimately empty. Leave them NA instead: the Excel
      # cell renders blank and the returned frame carries NA for pct and moe.
      #
      # This is deliberately narrow. It only fires when EVERY value of the
      # item is missing in this column, which is a structural empty, and only
      # under na_rm = TRUE. With na_rm = FALSE, get_freqs() reports missingness
      # as its own category rather than erroring, so an all-missing item is a
      # legitimate 100% result and must not be skipped. A row value that is
      # merely absent from a non-empty item still records 0 below, as before.
      if (na_rm && base::all(base::is.na(col_data_vars[[var]]))) {
        next
      }

      # Call get_freqs() for this row variable within this subgroup.
      # For plain data frames, pass wt_var via the wt argument.
      # For survey designs, weights are taken from the design automatically.
      freqs <- if (is_survey) {
        get_freqs(
          data = col_data,
          x = tidyselect::all_of(var),
          na.rm = na_rm
        )
      } else {
        get_freqs(
          data = col_data,
          x = tidyselect::all_of(var),
          wt = !!rlang::sym(wt_var),
          na.rm = na_rm
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
  # Built in one pass. The obvious loop makes a one-row data.frame per cell and
  # rbind()s them, which on a large sheet means tens of thousands of
  # data.frame() calls -- pure overhead, since the Excel output is written from
  # crosstab_matrix and never from this frame.
  #
  # Column-major indexing reproduces the loop order exactly (j outer, i inner),
  # which matters because the order() below is stable and ties keep their
  # incoming order.
  col_idx <- base::rep(1:num_cols, each = num_rows)
  row_idx <- base::rep(1:num_rows, times = num_cols)

  pct_percentage <- crosstab_matrix[base::cbind(row_idx, col_idx)]

  col_chr <- function(field) {
    base::vapply(
      columns_list,
      function(col) base::as.character(col[[field]]),
      character(1)
    )[col_idx]
  }
  row_chr <- function(field) {
    base::vapply(
      rows_list,
      function(r) base::as.character(r[[field]]),
      character(1)
    )[row_idx]
  }

  n_vec <- base::vapply(
    columns_list,
    function(col) base::as.numeric(col$unweighted_n),
    numeric(1)
  )[col_idx]
  deff_vec <- base::vapply(
    columns_list,
    function(col) base::as.numeric(col$deff),
    numeric(1)
  )[col_idx]

  results_df <- base::data.frame(
    subgroup_var = col_chr("var_name"),
    subgroup_var_label = col_chr("var_label"),
    subgroup_val = col_chr("value"),
    n = n_vec,
    row_var = row_chr("var_name"),
    row_var_label = row_chr("var_label"),
    row_val = row_chr("value"),
    pct = pct_percentage / 100,
    moe = calc_moe_vec(pct_percentage, n_vec, deff_vec),
    stringsAsFactors = FALSE
  )

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
  # Each question gets an n row above its response rows, except where that
  # would just repeat the row above. An item's collapse carries the same base
  # as the item it collapses, so writing it twice adds a row and says nothing.
  #
  # A collapse is recognised the same way the row borders already recognise
  # one -- an identical variable label on the row variable directly above --
  # and only when the bases genuinely match. Requiring both means a shared
  # label never hides a base that actually differs.
  row_var_labels <- base::vapply(
    row_var_names,
    function(v) {
      for (r in rows_list) {
        if (r$var_name == v) {
          return(r$var_label)
        }
      }
      v
    },
    character(1)
  )

  show_n_row <- base::rep(TRUE, base::length(row_var_names))
  if (base::length(row_var_names) > 1) {
    for (k in 2:base::length(row_var_names)) {
      repeats_label <- row_var_labels[[k]] == row_var_labels[[k - 1]]
      repeats_base <- base::identical(
        base::unname(question_n[k, ]),
        base::unname(question_n[k - 1, ])
      )
      show_n_row[[k]] <- !(repeats_label && repeats_base)
    }
  }

  # One entry per output row: either a question's n row or one response row.
  # Built so the sheet can carry rows the percentage matrix does not, while
  # rows_list and the returned data frame stay exactly as they were.
  sheet_plan <- base::list()

  for (k in base::seq_along(row_var_names)) {
    if (show_n_row[[k]]) {
      sheet_plan[[base::length(sheet_plan) + 1]] <- base::list(
        var_name = row_var_names[[k]],
        var_label = row_var_labels[[k]],
        value = "n (unweighted)",
        values = question_n[k, ],
        is_n = TRUE
      )
    }

    for (i in 1:num_rows) {
      if (rows_list[[i]]$var_name != row_var_names[[k]]) {
        next
      }
      sheet_plan[[base::length(sheet_plan) + 1]] <- base::list(
        var_name = rows_list[[i]]$var_name,
        var_label = rows_list[[i]]$var_label,
        value = base::as.character(rows_list[[i]]$value),
        # Divide by 100: Excel's "0%" format expects a decimal (0.30 -> "30%")
        values = crosstab_matrix[i, ] / 100,
        is_n = FALSE
      )
    }
  }

  num_sheet_rows <- base::length(sheet_plan)

  # Sheet rows holding a count rather than a percentage, as Excel row numbers.
  # They need their own number format, applied after the percentage format
  # below, or a base of 671 renders as 67100%.
  n_row_numbers <- base::which(
    base::vapply(sheet_plan, function(p) p$is_n, logical(1))
  ) + 3

  data_rows_text <- base::matrix(NA, nrow = num_sheet_rows, ncol = 2)
  data_rows_numeric <- base::matrix(NA, nrow = num_sheet_rows, ncol = num_cols)

  for (i in 1:num_sheet_rows) {
    data_rows_text[i, 1] <- sheet_plan[[i]]$var_label
    data_rows_text[i, 2] <- sheet_plan[[i]]$value
    data_rows_numeric[i, ] <- sheet_plan[[i]]$values
  }

  # ============================================================================
  # Create Excel Workbook
  # ============================================================================

  # A caller-supplied workbook is used as-is. Reading file_name back off disk
  # here is what makes multi-sheet builds slow, so skip it entirely in that
  # case -- see the multi-sheet note in Details.
  if (!base::is.null(wb)) {
    if (!base::inherits(wb, "wbWorkbook")) {
      stop(
        "`wb` must be an openxlsx2 wbWorkbook object, such as ",
        "attr(<previous result>, \"wb\"), not a ",
        base::class(wb)[[1]],
        "."
      )
    }

    if (sheet_name %in% openxlsx2::wb_get_sheet_names(wb)) {
      if (overwrite_existing_sheet) {
        wb <- openxlsx2::wb_remove_worksheet(wb, sheet = sheet_name)
        wb <- openxlsx2::wb_add_worksheet(wb, sheet = sheet_name)
      } else {
        stop(
          "Sheet '",
          sheet_name,
          "' already exists in the workbook passed as `wb`. ",
          "Set overwrite_existing_sheet = TRUE to overwrite."
        )
      }
    } else {
      wb <- openxlsx2::wb_add_worksheet(wb, sheet = sheet_name)
    }
  } else if (base::file.exists(file_name)) {
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
    dims = openxlsx2::wb_dims(rows = 4:(num_sheet_rows + 3), cols = 1:2),
    color = gray
  )
  wb <- openxlsx2::wb_add_font(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 4:(num_sheet_rows + 3), cols = 1:2),
    bold = TRUE
  )
  wb <- openxlsx2::wb_add_cell_style(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(rows = 4:(num_sheet_rows + 3), cols = 1:2),
    horizontal = "center",
    vertical = "center",
    wrap_text = TRUE
  )

  # ---- White fill + percentage format + center align: data cells ----
  wb <- openxlsx2::wb_add_fill(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(
      rows = 4:(num_sheet_rows + 3),
      cols = 3:(num_cols + 2)
    ),
    color = white
  )
  wb <- openxlsx2::wb_add_numfmt(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(
      rows = 4:(num_sheet_rows + 3),
      cols = 3:(num_cols + 2)
    ),
    numfmt = "0%"
  )
  wb <- openxlsx2::wb_add_cell_style(
    wb,
    sheet = sheet_name,
    dims = openxlsx2::wb_dims(
      rows = 4:(num_sheet_rows + 3),
      cols = 3:(num_cols + 2)
    ),
    horizontal = "center",
    vertical = "center"
  )

  # ---- Counts, not percentages: the per-question n rows ----
  # Applied after the percentage format above so it wins on these rows. One
  # call over a row vector rather than one per row: wb_add_numfmt() rewrites
  # entries in a style table that grows as it goes, so per-row calls are the
  # expensive pattern here.
  if (base::length(n_row_numbers) > 0) {
    wb <- openxlsx2::wb_add_numfmt(
      wb,
      sheet = sheet_name,
      dims = openxlsx2::wb_dims(
        rows = n_row_numbers,
        cols = 3:(num_cols + 2)
      ),
      numfmt = "0"
    )
  }

  # ============================================================================
  # Add Borders Between Variable Groups
  # ============================================================================

  # IMPORTANT: update = TRUE merges the new border side onto any existing
  # border styles on those cells, rather than overwriting all sides.
  # Without this, applying a top border would clear any left/right borders
  # that were already applied to the same cells.

  black <- openxlsx2::wb_color(hex = "FF000000")

  # Horizontal borders between row variables.
  #
  # These two loops are the dominant cost on a large sheet: each
  # wb_add_border(update = TRUE) reads and rewrites entries in a cellXfs style
  # table that grows as it goes, so ~130 row calls plus ~20 column calls cost
  # far more than the number of cells involved would suggest.
  #
  # They cannot be collapsed by passing a multi-range dims. wb_dims() will
  # happily build one ("A5:BF5,A9:BF9"), but wb_add_border() treats its dims as
  # a single rectangle whose edges take different styles, so a multi-range
  # either fails with "style length and name do not match" or silently leaves
  # most of the targeted cells with no border at all. This was tried and
  # reverted.
  #
  # Note for anyone retrying: cell values and style-table counts are NOT
  # sufficient evidence. The broken version produced identical cell values and
  # identical cellXfs/border counts while dropping the border on 316 of 931
  # cells. Resolve each cell through cellXfs to its border definition and
  # compare that.
  # Walks sheet_plan, not rows_list: the sheet carries an n row per question
  # that the percentage matrix does not, so a rows_list index no longer maps
  # to an Excel row.
  current_var <- sheet_plan[[1]]$var_name
  current_label <- sheet_plan[[1]]$var_label

  for (i in 2:num_sheet_rows) {
    if (sheet_plan[[i]]$var_name != current_var) {
      excel_row <- i + 3
      border_style <- if (sheet_plan[[i]]$var_label == current_label) {
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

      current_var <- sheet_plan[[i]]$var_name
      current_label <- sheet_plan[[i]]$var_label
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
        dims = openxlsx2::wb_dims(
          rows = 1:(num_sheet_rows + 3),
          cols = excel_col
        ),
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
    dims = openxlsx2::wb_dims(rows = 1:(num_sheet_rows + 3), cols = 2),
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

  for (i in 1:num_sheet_rows) {
    excel_row <- i + 3
    if (i == num_sheet_rows || data_rows_text[i + 1, 1] != current_label) {
      if (excel_row > start_row) {
        wb <- openxlsx2::wb_merge_cells(
          wb,
          sheet = sheet_name,
          dims = openxlsx2::wb_dims(rows = start_row:excel_row, cols = 1)
        )
      }
      if (i < num_sheet_rows) {
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

  if (save) {
    openxlsx2::wb_save(wb, file = file_name, overwrite = TRUE)

    base::message(
      "Crosstabs successfully exported to: ",
      file_name,
      " (Sheet: ",
      sheet_name,
      ")"
    )
  } else {
    base::message(
      "Crosstabs added to workbook (Sheet: ",
      sheet_name,
      "). Not saved -- pass attr(result, \"wb\") to the next call, ",
      "then save the workbook when the last sheet is added."
    )
  }

  # Attached so a caller can chain sheets without a disk round trip.
  base::attr(results_df, "wb") <- wb

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
