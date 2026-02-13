#' Create Polling Crosstabs and Export to Excel
#'
#' This function takes survey data and creates formatted crosstabs showing
#' weighted percentages of row variables across different subgroups. The results
#' are exported to an Excel file with proper formatting.
#'
#' @param data A data frame or survey design object containing survey responses
#' @param row_vars Character vector of variable names to display as rows
#' @param subgroup_vars Character vector of variable names to use as column subgroups
#' @param wt_var Character string specifying the name of the weight variable
#' @param min_n Integer specifying minimum unweighted sample size for a subgroup
#'   to be included (default: 75)
#' @param file_name Character string specifying the Excel file path/name
#' @param sheet_name Character string specifying the sheet name
#' @param overwrite_existing_sheet Logical indicating whether to overwrite existing
#'   sheet with same name (default: TRUE)
#'
#' @return Invisibly returns the crosstab data structure; writes Excel file as side effect
#'
#' @examples
#' \dontrun{
#' create_polling_crosstabs(
#'   data = survey_data,
#'   row_vars = c("job_approval", "direction"),
#'   subgroup_vars = c("gender", "race"),
#'   wt_var = "weight",
#'   min_n = 75,
#'   file_name = "crosstabs.xlsx",
#'   sheet_name = "Results"
#' )
#' }
#'
#' @export
create_polling_crosstabs <- function(data,
                                     row_vars,
                                     subgroup_vars,
                                     wt_var,
                                     min_n = 75,
                                     file_name,
                                     sheet_name,
                                     overwrite_existing_sheet = TRUE) {
  
  # ============================================================================
  # Load Required Packages
  # ============================================================================
  
  if (!require("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. Please install it with: install.packages('openxlsx')")
  }
  
  # ============================================================================
  # Input Validation
  # ============================================================================
  
  # Ensure data is a data frame
  if (inherits(data, "survey.design")) {
    # If survey design object, extract the variables component
    data <- data$variables
  }
  data <- as.data.frame(data)
  
  # Check that all specified variables exist in the data
  all_vars <- c(row_vars, subgroup_vars, wt_var)
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("The following variables are not found in the data: ", 
         paste(missing_vars, collapse = ", "))
  }
  
  # Check that weights are numeric
  if (!is.numeric(data[[wt_var]])) {
    stop("Weight variable '", wt_var, "' must be numeric")
  }
  
  # ============================================================================
  # Helper Functions
  # ============================================================================
  
  #' Calculate weighted percentage
  #' @param values Vector of values (typically a factor or character)
  #' @param weights Vector of weights corresponding to values
  #' @return Named vector of weighted percentages
  calc_weighted_pct <- function(values, weights) {
    # Remove NAs
    valid_idx <- !is.na(values) & !is.na(weights)
    values <- values[valid_idx]
    weights <- weights[valid_idx]
    
    if (length(values) == 0) {
      return(numeric(0))
    }
    
    # Calculate weighted counts
    unique_vals <- unique(values)
    weighted_counts <- sapply(unique_vals, function(val) {
      sum(weights[values == val])
    })
    
    # Convert to percentages
    total_weight <- sum(weighted_counts)
    if (total_weight == 0) {
      return(setNames(rep(0, length(unique_vals)), unique_vals))
    }
    
    weighted_pct <- (weighted_counts / total_weight) * 100
    names(weighted_pct) <- unique_vals
    
    return(weighted_pct)
  }
  
  #' Get variable label or use variable name if no label exists
  #' @param data Data frame
  #' @param var_name Variable name
  #' @return Variable label or name
  get_var_label <- function(data, var_name) {
    label <- attr(data[[var_name]], "label")
    if (is.null(label) || length(label) == 0 || label == "") {
      return(var_name)
    }
    return(label)
  }
  
  # ============================================================================
  # Generate Subgroup Combinations
  # ============================================================================
  
  # Create a data frame with all combinations of subgroups
  subgroup_combos <- expand.grid(
    lapply(subgroup_vars, function(var) unique(data[[var]])),
    stringsAsFactors = FALSE
  )
  names(subgroup_combos) <- subgroup_vars
  
  # Add an "Overall" column (all respondents)
  overall_df <- data.frame(Overall = "Overall", stringsAsFactors = FALSE)
  
  # ============================================================================
  # Build Column Structure (Subgroups)
  # ============================================================================
  
  columns_list <- list()
  
  # Add Overall column first
  columns_list[[1]] <- list(
    var_name = "Overall",
    var_label = "Overall",
    value = "Overall",
    data_subset = data,
    unweighted_n = nrow(data)
  )
  
  # Add each subgroup combination
  for (var in subgroup_vars) {
    var_label <- get_var_label(data, var)
    unique_values <- sort(unique(data[[var]][!is.na(data[[var]])]))
    
    for (val in unique_values) {
      subset_data <- data[!is.na(data[[var]]) & data[[var]] == val, ]
      unweighted_n <- nrow(subset_data)
      
      # Only include if meets minimum n threshold
      if (unweighted_n >= min_n) {
        columns_list[[length(columns_list) + 1]] <- list(
          var_name = var,
          var_label = var_label,
          value = val,
          data_subset = subset_data,
          unweighted_n = unweighted_n
        )
      }
    }
  }
  
  num_cols <- length(columns_list)
  
  # ============================================================================
  # Build Row Structure (Questions)
  # ============================================================================
  
  rows_list <- list()
  
  for (var in row_vars) {
    var_label <- get_var_label(data, var)
    unique_values <- unique(data[[var]][!is.na(data[[var]])])
    
    # If the variable is a factor, use factor levels for ordering
    if (is.factor(data[[var]])) {
      unique_values <- levels(data[[var]])
      unique_values <- unique_values[unique_values %in% data[[var]]]
    }
    
    for (val in unique_values) {
      rows_list[[length(rows_list) + 1]] <- list(
        var_name = var,
        var_label = var_label,
        value = val
      )
    }
  }
  
  num_rows <- length(rows_list)
  
  # ============================================================================
  # Calculate Weighted Percentages
  # ============================================================================
  
  # Create matrix to hold percentages
  crosstab_matrix <- matrix(NA, nrow = num_rows, ncol = num_cols)
  
  for (i in 1:num_rows) {
    row_var <- rows_list[[i]]$var_name
    row_val <- rows_list[[i]]$value
    
    for (j in 1:num_cols) {
      subset_data <- columns_list[[j]]$data_subset
      
      # Get the values and weights for this subgroup
      values <- subset_data[[row_var]]
      weights <- subset_data[[wt_var]]
      
      # Calculate weighted percentage
      weighted_pcts <- calc_weighted_pct(values, weights)
      
      # Extract the percentage for this specific row value
      if (row_val %in% names(weighted_pcts)) {
        crosstab_matrix[i, j] <- weighted_pcts[row_val]
      } else {
        crosstab_matrix[i, j] <- 0
      }
    }
  }
  
  # ============================================================================
  # Build Excel Structure
  # ============================================================================
  
  # Create header rows
  # Row 1: Subgroup variable labels (will be merged)
  # Row 2: Subgroup values
  # Row 3: Unweighted n
  
  header_row1 <- c("", "", rep(NA, num_cols))  # Placeholders for index columns
  header_row2 <- c("", "", rep(NA, num_cols))
  header_row3 <- c("", "n (unweighted)", rep(NA, num_cols))
  
  for (j in 1:num_cols) {
    header_row1[j + 2] <- columns_list[[j]]$var_label
    header_row2[j + 2] <- as.character(columns_list[[j]]$value)
    header_row3[j + 2] <- columns_list[[j]]$unweighted_n
  }
  
  # Create data rows with index columns
  # Convert percentages to decimal format for Excel percentage formatting
  data_rows <- matrix(NA, nrow = num_rows, ncol = num_cols + 2)
  
  for (i in 1:num_rows) {
    data_rows[i, 1] <- rows_list[[i]]$var_label
    data_rows[i, 2] <- as.character(rows_list[[i]]$value)
    # Divide by 100 to convert to decimal for Excel percentage format
    data_rows[i, 3:(num_cols + 2)] <- crosstab_matrix[i, ] / 100
  }
  
  # ============================================================================
  # Create Excel Workbook and Apply Formatting
  # ============================================================================
  
  # Check if file exists
  if (file.exists(file_name)) {
    # Load existing workbook
    wb <- loadWorkbook(file_name)
    
    # Check if sheet exists
    if (sheet_name %in% names(wb)) {
      if (overwrite_existing_sheet) {
        removeWorksheet(wb, sheet_name)
        addWorksheet(wb, sheet_name)
      } else {
        stop("Sheet '", sheet_name, "' already exists in '", file_name, 
             "'. Set overwrite_existing_sheet = TRUE to overwrite.")
      }
    } else {
      addWorksheet(wb, sheet_name)
    }
  } else {
    # Create new workbook
    wb <- createWorkbook()
    addWorksheet(wb, sheet_name)
  }
  
  # Write header rows (rows 1-3)
  writeData(wb, sheet = sheet_name, x = t(header_row1), 
            startRow = 1, startCol = 1, colNames = FALSE)
  writeData(wb, sheet = sheet_name, x = t(header_row2), 
            startRow = 2, startCol = 1, colNames = FALSE)
  writeData(wb, sheet = sheet_name, x = t(header_row3), 
            startRow = 3, startCol = 1, colNames = FALSE)
  
  # Write data rows (starting at row 4)
  for (i in 1:num_rows) {
    # Write index columns as text
    writeData(wb, sheet = sheet_name, x = data_rows[i, 1], 
              startRow = i + 3, startCol = 1, colNames = FALSE)
    writeData(wb, sheet = sheet_name, x = data_rows[i, 2], 
              startRow = i + 3, startCol = 2, colNames = FALSE)
    
    # Write numeric data columns (ensure they're numeric for percentage formatting)
    numeric_values <- as.numeric(data_rows[i, 3:(num_cols + 2)])
    writeData(wb, sheet = sheet_name, x = t(numeric_values), 
              startRow = i + 3, startCol = 3, colNames = FALSE)
  }
  
  # ============================================================================
  # Apply Cell Styling
  # ============================================================================
  
  # Create styles
  gray_header_style <- createStyle(fgFill = "#D3D3D3", 
                                   halign = "center", 
                                   valign = "center",
                                   textDecoration = "bold",
                                   wrapText = TRUE)
  
  gray_index_style <- createStyle(fgFill = "#D3D3D3", 
                                  halign = "center", 
                                  valign = "center",
                                  textDecoration = "bold",
                                  wrapText = TRUE)
  
  white_header_style <- createStyle(fgFill = "#FFFFFF",
                                    halign = "center",
                                    valign = "center",
                                    textDecoration = "bold",
                                    wrapText = TRUE)
  
  white_data_style <- createStyle(fgFill = "#FFFFFF",
                                  halign = "center",
                                  valign = "center")
  
  # Percentage style for data cells (whole percentages, no decimal)
  percentage_style <- createStyle(fgFill = "#FFFFFF",
                                  halign = "center",
                                  valign = "center",
                                  numFmt = "0%")
  
  # Apply gray style to top 2 header rows (all columns)
  addStyle(wb, sheet = sheet_name, style = gray_header_style, 
           rows = 1:2, cols = 1:(num_cols + 2), gridExpand = TRUE, stack = TRUE)
  
  # Apply GRAY style to row 3 (n row) for index columns
  addStyle(wb, sheet = sheet_name, style = gray_header_style,
           rows = 3, cols = 1:2, gridExpand = TRUE, stack = TRUE)
  
  # Apply WHITE style to row 3 (n row) for data columns
  addStyle(wb, sheet = sheet_name, style = white_data_style,
           rows = 3, cols = 3:(num_cols + 2), gridExpand = TRUE, stack = TRUE)
  
  # Apply gray style to left 2 index columns (data rows only)
  addStyle(wb, sheet = sheet_name, style = gray_index_style, 
           rows = 4:(num_rows + 3), cols = 1:2, gridExpand = TRUE, stack = TRUE)
  
  # Apply percentage style to data cells
  addStyle(wb, sheet = sheet_name, style = percentage_style,
           rows = 4:(num_rows + 3), cols = 3:(num_cols + 2), gridExpand = TRUE, stack = TRUE)
  
  # ============================================================================
  # Add Borders Between Variable Groups
  # ============================================================================
  
  # Create border styles
  # Solid medium border for different variable labels
  top_border_solid <- createStyle(border = "top", borderColour = "#000000", borderStyle = "medium")
  left_border_solid <- createStyle(border = "left", borderColour = "#000000", borderStyle = "medium")
  
  # Dashed thin border for same variable labels (different variables)
  top_border_dashed <- createStyle(border = "top", borderColour = "#000000", borderStyle = "dashed")
  left_border_dashed <- createStyle(border = "left", borderColour = "#000000", borderStyle = "dashed")
  
  bottom_border_style <- createStyle(border = "bottom", borderColour = "#000000", borderStyle = "medium")
  right_border_style <- createStyle(border = "right", borderColour = "#000000", borderStyle = "medium")
  
  # Add horizontal borders between different row variables
  current_var <- rows_list[[1]]$var_name
  current_label <- rows_list[[1]]$var_label
  
  for (i in 2:num_rows) {
    if (rows_list[[i]]$var_name != current_var) {
      # Add border at the top of this row
      excel_row <- i + 3
      
      # Check if the label is the same as the previous variable
      # If same label but different variable, use dashed thin border
      # If different label, use solid medium border
      if (rows_list[[i]]$var_label == current_label) {
        # Same label, different variable - use dashed border
        addStyle(wb, sheet = sheet_name, style = top_border_dashed,
                 rows = excel_row, cols = 1:(num_cols + 2), 
                 gridExpand = TRUE, stack = TRUE)
      } else {
        # Different label - use solid border
        addStyle(wb, sheet = sheet_name, style = top_border_solid,
                 rows = excel_row, cols = 1:(num_cols + 2), 
                 gridExpand = TRUE, stack = TRUE)
      }
      
      current_var <- rows_list[[i]]$var_name
      current_label <- rows_list[[i]]$var_label
    }
  }
  
  # Add vertical borders between different subgroup variables
  current_var <- columns_list[[1]]$var_name
  current_label <- columns_list[[1]]$var_label
  
  for (j in 2:num_cols) {
    if (columns_list[[j]]$var_name != current_var) {
      # Add border to the left of this column
      excel_col <- j + 2
      
      # Check if the label is the same as the previous variable
      # If same label but different variable, use dashed thin border
      # If different label, use solid medium border
      if (columns_list[[j]]$var_label == current_label) {
        # Same label, different variable - use dashed border
        addStyle(wb, sheet = sheet_name, style = left_border_dashed,
                 rows = 1:(num_rows + 3), cols = excel_col, 
                 gridExpand = TRUE, stack = TRUE)
      } else {
        # Different label - use solid border
        addStyle(wb, sheet = sheet_name, style = left_border_solid,
                 rows = 1:(num_rows + 3), cols = excel_col, 
                 gridExpand = TRUE, stack = TRUE)
      }
      
      current_var <- columns_list[[j]]$var_name
      current_label <- columns_list[[j]]$var_label
    }
  }
  
  # Add right border to the right of index columns (columns 1 and 2)
  addStyle(wb, sheet = sheet_name, style = right_border_style,
           rows = 1:(num_rows + 3), cols = 2, gridExpand = TRUE, stack = TRUE)
  
  # Add bottom border below header rows (row 2)
  addStyle(wb, sheet = sheet_name, style = bottom_border_style,
           rows = 2, cols = 1:(num_cols + 2), gridExpand = TRUE, stack = TRUE)
  
  # Add bottom border below n-sizes row (row 3)
  addStyle(wb, sheet = sheet_name, style = bottom_border_style,
           rows = 3, cols = 1:(num_cols + 2), gridExpand = TRUE, stack = TRUE)
  
  # ============================================================================
  # Merge Cells
  # ============================================================================
  
  # Merge top-left 4 cells (a1:b2)
  mergeCells(wb, sheet = sheet_name, rows = 1:2, cols = 1:2)
  
  # Merge cells for subgroup variable labels (header row 1)
  # This groups consecutive columns that have the same variable label
  # Note: If two different subgroup variables have the same label, their columns
  # will be merged together (e.g., if both "gender" and "gender_recode" have 
  # the label "Gender", all their columns will share one merged header cell)
  current_label <- header_row1[3]
  start_col <- 3
  
  for (j in 3:(num_cols + 2)) {
    if (j == (num_cols + 2) || header_row1[j + 1] != current_label) {
      # Merge from start_col to j
      if (j > start_col) {
        mergeCells(wb, sheet = sheet_name, rows = 1, cols = start_col:j)
      }
      # Update for next group
      if (j < (num_cols + 2)) {
        current_label <- header_row1[j + 1]
        start_col <- j + 1
      }
    }
  }
  
  # Merge cells for row variable labels (first index column)
  # This groups consecutive rows that have the same variable label
  # Note: If two different row variables have the same label, their rows will
  # be merged together (e.g., if both "approval_q1" and "approval_q2" have the
  # label "Job Approval", all their rows will share one merged cell in column 1)
  current_label <- data_rows[1, 1]
  start_row <- 4  # Data starts at row 4
  
  for (i in 1:num_rows) {
    excel_row <- i + 3
    if (i == num_rows || data_rows[i + 1, 1] != current_label) {
      # Merge from start_row to excel_row
      if (excel_row > start_row) {
        mergeCells(wb, sheet = sheet_name, rows = start_row:excel_row, cols = 1)
      }
      # Update for next group
      if (i < num_rows) {
        current_label <- data_rows[i + 1, 1]
        start_row <- excel_row + 1
      }
    }
  }
  
  # ============================================================================
  # Set Column Widths
  # ============================================================================
  
  # Set width for index columns
  setColWidths(wb, sheet = sheet_name, cols = 1, widths = 50)
  setColWidths(wb, sheet = sheet_name, cols = 2, widths = 40)
  
  # Set width for data columns (all 20)
  setColWidths(wb, sheet = sheet_name, cols = 3:(num_cols + 2), widths = 20)
  
  # ============================================================================
  # Freeze Panes
  # ============================================================================
  
  # Freeze the top 2 rows and left 2 columns
  # The freeze occurs at cell C3, meaning rows 1-2 and columns A-B remain frozen
  # firstActiveRow = 3 means row 3 is the first row that scrolls
  # firstActiveCol = 3 means column C is the first column that scrolls
  freezePane(wb, sheet = sheet_name, firstActiveRow = 3, firstActiveCol = 3)
  
  # ============================================================================
  # Save Workbook
  # ============================================================================
  
  saveWorkbook(wb, file_name, overwrite = TRUE)
  
  message("Crosstabs successfully exported to: ", file_name, " (Sheet: ", sheet_name, ")")
  
  # Return the crosstab structure invisibly
  invisible(list(
    columns = columns_list,
    rows = rows_list,
    crosstab_matrix = crosstab_matrix
  ))
}


# ============================================================================
# Example Usage
# ============================================================================

#' Example of how to use the create_polling_crosstabs function
#'
#' @examples
#' \dontrun{
#' # Create sample survey data
#' set.seed(123)
#' n <- 500
#' survey_data <- data.frame(
#'   gender = sample(c("Male", "Female"), n, replace = TRUE),
#'   race = sample(c("White", "Black", "Hispanic", "Other"), n, 
#'                 replace = TRUE, prob = c(0.6, 0.15, 0.15, 0.1)),
#'   job_approval = sample(c("Strongly approve", "Somewhat approve", 
#'                          "Somewhat disapprove", "Strongly disapprove"), 
#'                        n, replace = TRUE),
#'   right_direction = sample(c("Right direction", "Wrong track"), 
#'                           n, replace = TRUE),
#'   weight = runif(n, 0.5, 2.0)
#' )
#'
#' # Add variable labels
#' attr(survey_data$gender, "label") <- "Gender"
#' attr(survey_data$race, "label") <- "Race/Ethnicity"
#' attr(survey_data$job_approval, "label") <- "Job Approval"
#' attr(survey_data$right_direction, "label") <- "Country Direction"
#'
#' # Create crosstabs
#' create_polling_crosstabs(
#'   data = survey_data,
#'   row_vars = c("job_approval", "right_direction"),
#'   subgroup_vars = c("gender", "race"),
#'   wt_var = "weight",
#'   min_n = 75,
#'   file_name = "polling_crosstabs.xlsx",
#'   sheet_name = "January 2026",
#'   overwrite_existing_sheet = TRUE
#' )
#' }
