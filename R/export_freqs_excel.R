#' Export Frequency Tables to Excel with Professional Formatting
#'
#' Creates professionally formatted frequency tables in Excel with support for
#' hierarchical headers, multiple grouping variables, and multi-sheet workbooks.
#' Tables include proper percentage formatting, borders, footnotes, and optional
#' comments explaining grouping variables.
#'
#' @param data A data frame or tibble containing the variables to analyze
#' @param cols <[`tidy-select`][dplyr_tidy_select]> Column selection for variables
#'   to create frequency tables for.
#' @param group <[`tidy-select`][dplyr_tidy_select]> Optional grouping variables
#'   for cross-tabulation. Can be a single variable name or vector of variable
#'   names. Supports multiple levels of grouping which will create hierarchical
#'   column headers
#' @param wt Optional weight variable name for weighted frequencies. If not provided,
#'   equal weights are applied to all observations
#' @param drop_zero Logical. Whether to drop categories with zero frequencies.
#'   Default is `FALSE`
#' @param decimals Integer. Number of decimal places to display for percentages.
#'   Default is `1`
#' @param na.rm Logical. Whether to remove missing values before calculating
#'   frequencies. Default is `TRUE`
#' @param show_genpop Logical. Whether to include a "General Population" column
#'   showing overall frequencies. Default is `FALSE`
#' @param file_name Character string specifying the output Excel file path
#' @param wb_subject Character string for workbook subject metadata. Default is `""`
#' @param wb_category Character string for workbook category metadata
#' @param add_comments Logical. Whether to add hover comments to group headers
#'   explaining what each grouping variable represents. Default is `TRUE`
#' @param sheet Character string specifying the worksheet name. Default is `"Frequencies"`
#' @param append_to_existing Logical. Whether to add to an existing Excel file
#'   (if it exists) or create a new one. Default is `TRUE`
#'
#' @return Invisibly returns `NULL`. The function is called for its side effect
#'   of creating an Excel file
#'
#' @details
#' The function creates professional frequency tables with the following features:
#' \itemize{
#'   \item \strong{Hierarchical headers}: Multiple grouping variables create
#'     nested column headers with proper spanning
#'   \item \strong{Smart formatting}: Percentages are formatted as actual Excel
#'     percentages (not text) to avoid warnings and enable calculations
#'   \item \strong{Professional styling}: Tables include borders, centered headers,
#'     and color-coded sections
#'   \item \strong{Informative footnotes}: Automatically generated notes explain
#'     what each level of grouping represents
#'   \item \strong{Multi-sheet support}: Can append new sheets to existing files
#'     with automatic sheet name incrementing to avoid conflicts
#'   \item \strong{Optional comments}: Hover comments on headers provide additional
#'     context about grouping variables
#' }
#'
#' When using multiple grouping variables, column names are created by joining
#' group values with underscores (e.g., "Gen_Z_Male_pct"). The function automatically
#' creates hierarchical headers that make these relationships clear.
#'
#' @section Multi-sheet Usage:
#' To create multiple sheets in the same Excel file:
#' \preformatted{
#' # First call creates the file
#' export_freqs_excel(data, vars, group = "generation",
#'                    file_name = "analysis.xlsx", sheet = "By Generation")
#'
#' # Subsequent calls add new sheets
#' export_freqs_excel(data, vars, group = c("generation", "gender"),
#'                    file_name = "analysis.xlsx", sheet = "By Gen and Gender")
#' }
#'
#' @examples
#' \dontrun{
#' # Basic frequency table
#' export_freqs_excel(survey_data,
#'                    cols = c(political_party, ideology),
#'                    file_name = "frequencies.xlsx")
#'
#' # Grouped by single variable
#' export_freqs_excel(survey_data,
#'                    cols = c(political_party, ideology),
#'                    group = generation,
#'                    file_name = "by_generation.xlsx")
#'
#' # Multiple grouping variables with hierarchical headers
#' export_freqs_excel(survey_data,
#'                    cols = political_party,
#'                    group = c(generation, has_children),
#'                    show_genpop = TRUE,
#'                    file_name = "detailed_analysis.xlsx")
#'
#' # Add to existing file with new sheet
#' export_freqs_excel(survey_data,
#'                    cols = political_party,
#'                    group = education,
#'                    file_name = "detailed_analysis.xlsx",
#'                    sheet = "By Education")
#' }
#'
#' @seealso
#' `get_freqs()` for the underlying frequency calculation function
#'
#' @export
export_freqs_excel <- function(
  data,
  cols,
  group = NULL,
  wt = NULL,
  drop_zero = FALSE,
  decimals = 1,
  na.rm = TRUE,
  show_genpop = FALSE,
  file_name,
  wb_subject = "",
  wb_category,
  add_comments = TRUE,
  sheet = "Frequencies",
  append_to_existing = TRUE
) {
  # --- Prepare Weights ---
  if (missing(wt)) {
    wt <- "wts"
    data[[wt]] <- rep(1, nrow(data))
  } else {
    wt <- rlang::as_name(rlang::ensym(wt))

    if (!is.numeric(data[[wt]])) {
      cli::cli_abort(c(
        "`{wt}` must be a numeric variable.",
        x = "Supplied variable is {class(data[[wt]])}."
      ))
    } else {
      data[[wt]][is.na(data[[wt]])] <- 0
    }
  }

  # --- Prepare Grouping Variables ---
  group_names <- if (inherits(data, "grouped_df")) {
    setdiff(names(attr(data, "groups")), ".rows")
  } else {
    NULL
  }
  group_vars <- if (missing(group)) NULL else select_groups({{ group }}, data)
  group_vars <- group_vars[group_vars != "c"]
  group_names <- unique(c(group_names, group_vars))

  # --- Subset to Relevant Columns ---
  if (!missing(cols)) {
    data <- data %>%
      dplyr::select(tidyselect::all_of(group_names), {{ cols }}, wts)
  }
  data <- dplyr::ungroup(data)
  cols <- get_col_names(data, {{ cols }})

  # --- Generate Frequency Tables ---
  freq_list <- purrr::map(
    cols,
    \(var) {
      get_freqs(
        data,
        {{ var }},
        group = {{ group_names }},
        wt = {{ wt }},
        drop_zero = drop_zero,
        decimals = decimals,
        na.rm = na.rm
      )
    }
  )

  # Create or load workbook
  if (append_to_existing && file.exists(file_name)) {
    # Load existing workbook
    wb <- openxlsx2::wb_load(file_name)
    print(paste(
      "Loaded workbook with sheets:",
      paste(wb$get_sheet_names(), collapse = ", ")
    ))

    # Auto-increment sheet name if it exists
    original_sheet <- sheet
    counter <- 1
    while (sheet %in% wb$get_sheet_names()) {
      sheet <- paste0(original_sheet, " (", counter, ")")
      counter <- counter + 1
    }

    # Add the new sheet
    wb <- wb %>% openxlsx2::wb_add_worksheet(sheet = sheet)
  } else {
    # Create new workbook
    wb <- openxlsx2::wb_workbook(creator = "me") %>%
      openxlsx2::wb_add_worksheet(sheet = sheet)
  }

  # set the initial row to 1
  row_pointer <- 1

  for (freq in freq_list) {
    # get the variable label
    var_label <- attr(freq, "variable_label")
    group_labels <- attr(freq, "group_labels")
    x_var <- as.character(rlang::quo_squash(attr(freq, "variable_name")))

    # Create wide version of frequency table
    wide <- prepare_freq_for_excel(
      freq,
      show_genpop = show_genpop,
      decimals = decimals,
      wt = wt
    )
    header_dims <- openxlsx2::wb_dims(rows = row_pointer, cols = 1:ncol(wide))

    # STORE the title row for border calculations
    title_start_row <- row_pointer

    # create the title of the table
    wb <- wb %>%
      openxlsx2::wb_add_data(
        sheet = sheet,
        x = var_label,
        start_col = 1,
        start_row = row_pointer
      ) %>%
      openxlsx2::wb_merge_cells(dims = header_dims) %>%
      openxlsx2::wb_add_fill(
        dims = header_dims,
        color = openxlsx2::wb_color("#a1d9fd")
      ) %>%
      openxlsx2::wb_add_font(
        dims = header_dims,
        bold = TRUE
      )

    row_pointer <- row_pointer + 1
    header_cols <- colnames(wide)[-1]

    if (!is.null(group_names)) {
      # Use hierarchical headers
      header_result <- create_hierarchical_headers(
        wb,
        sheet,
        wide,
        row_pointer,
        group_names,
        group_labels,
        add_comments
      )

      wb <- header_result$wb
      header_rows <- header_result$rows_used

      row_pointer <- row_pointer + header_rows

      # Write the main data
      wb <- wb %>%
        openxlsx2::wb_add_data(
          sheet = sheet,
          x = wide,
          start_col = 1,
          start_row = row_pointer,
          col_names = FALSE
        )

      # Format percentage columns as actual percentages
      # figure out which columns have pct in them
      pct_cols <- which(grepl("pct", colnames(wide), ignore.case = TRUE))

      if (length(pct_cols) > 0) {
        # Convert percentage text to numbers and format as percentage
        for (col in pct_cols) {
          wb <- wb %>%
            openxlsx2::wb_add_numfmt(
              dims = openxlsx2::wb_dims(
                rows = row_pointer:(row_pointer + nrow(wide) - 1),
                cols = col
              ),
              numfmt = paste0(
                "0.",
                paste(rep("0", decimals), collapse = ""),
                "%"
              )
            )
        }
      }

      # Calculate borders AFTER data is written
      table_start_row <- title_start_row
      table_end_row <- row_pointer + nrow(wide) - 1
      table_start_col <- 1
      table_end_col <- ncol(wide)

      # add borders
      wb <- wb %>%
        openxlsx2::wb_add_border(
          dims = openxlsx2::wb_dims(
            rows = table_start_row:table_end_row,
            cols = table_start_col:table_end_col
          ),
          top_color = openxlsx2::wb_color("#000000"),
          top_border = "thick",
          bottom_color = openxlsx2::wb_color("#000000"),
          bottom_border = "thick",
          left_color = openxlsx2::wb_color("#000000"),
          left_border = "thick",
          right_color = openxlsx2::wb_color("#000000"),
          right_border = "thick"
        )

      # Add footnotes after the border code
      if (!is.null(group_names) && !is.null(group_labels)) {
        # Move down a bit from the table
        row_pointer <- row_pointer + nrow(wide) + 1

        # Create footnotes directly from group_names and group_labels
        explanation_lines <- c()

        for (i in seq_along(group_names)) {
          group_var <- group_names[i]
          group_label <- if (!is.null(group_labels[[group_var]])) {
            group_labels[[group_var]]
          } else {
            group_var
          }

          # Create footnote showing the hierarchy
          if (i == 1) {
            explanation_line <- paste0(
              "Note: Top level headers represent ",
              group_label
            )
          } else if (i == 2) {
            explanation_line <- paste0(
              "Note: Second level headers represent ",
              group_label
            )
          } else if (i == 3) {
            explanation_line <- paste0(
              "Note: Third level headers represent ",
              group_label
            )
          } else {
            explanation_line <- paste0(
              "Note: Level ",
              i,
              " headers represent ",
              group_label
            )
          }

          explanation_lines <- c(explanation_lines, explanation_line)
        }

        # Add each explanation line
        for (line in explanation_lines) {
          explanation_dims <- openxlsx2::wb_dims(
            rows = row_pointer,
            cols = 1:ncol(wide)
          )

          wb <- wb %>%
            openxlsx2::wb_add_data(
              sheet = sheet,
              x = line,
              start_col = 1,
              start_row = row_pointer
            ) %>%
            openxlsx2::wb_merge_cells(dims = explanation_dims) %>%
            openxlsx2::wb_add_font(
              dims = explanation_dims,
              size = 9,
              italic = TRUE,
              color = openxlsx2::wb_color("#666666")
            )

          row_pointer <- row_pointer + 1
        }

        # Add extra space before next table
        row_pointer <- row_pointer + 3
      } else {
        # For non-grouped data, just add the standard spacing
        row_pointer <- row_pointer + nrow(wide) + 5
      }
    } else {
      # No groups: just add simple one-row header
      header_row <- c(x_var, "Percent", "N") # Fixed variable name

      wb <- wb %>%
        openxlsx2::wb_add_data(
          sheet = sheet,
          x = matrix(header_row, nrow = 1), # Fixed variable name
          start_row = row_pointer,
          start_col = 1,
          col_names = FALSE
        ) %>%
        openxlsx2::wb_add_font(
          dims = openxlsx2::wb_dims(
            rows = row_pointer,
            cols = 1:length(header_row)
          ), # Fixed variable name
          bold = TRUE
        ) %>%
        openxlsx2::wb_add_fill(
          dims = openxlsx2::wb_dims(
            rows = row_pointer,
            cols = 1:length(header_row)
          ), # Fixed variable name
          color = openxlsx2::wb_color("#E6E6FA")
        )

      row_pointer <- row_pointer + 1

      # Write the main data
      wb <- wb %>%
        openxlsx2::wb_add_data(
          sheet = sheet,
          x = wide,
          start_col = 1,
          start_row = row_pointer,
          col_names = FALSE
        )

      # Format percentage columns as actual percentages
      # figure out which columns have pct in them
      pct_cols <- which(grepl("pct", colnames(wide), ignore.case = TRUE))

      if (length(pct_cols) > 0) {
        # Convert percentage text to numbers and format as percentage
        for (col in pct_cols) {
          wb <- wb %>%
            openxlsx2::wb_add_numfmt(
              dims = openxlsx2::wb_dims(
                rows = row_pointer:(row_pointer + nrow(wide) - 1),
                cols = col
              ),
              numfmt = paste0(
                "0.",
                paste(rep("0", decimals), collapse = ""),
                "%"
              )
            )
        }
      }

      # Calculate borders AFTER data is written
      table_start_row <- title_start_row # Start from title
      table_end_row <- row_pointer + nrow(wide) - 1
      table_start_col <- 1
      table_end_col <- ncol(wide)

      wb <- wb %>%
        openxlsx2::wb_add_border(
          # specify where to add borders
          dims = openxlsx2::wb_dims(
            # specify which rows are in the table
            rows = table_start_row:table_end_row,
            # specify which columns are
            cols = table_start_col:table_end_col
          ),
          # specify border colors and thicknesses
          top_color = openxlsx2::wb_color("#000000"),
          top_border = "thick",
          bottom_color = openxlsx2::wb_color("#000000"),
          bottom_border = "thick",
          left_color = openxlsx2::wb_color("#000000"),
          left_border = "thick",
          right_color = openxlsx2::wb_color("#000000"),
          right_border = "thick"
        )

      row_pointer <- row_pointer + nrow(wide) + 5
    }
  }

  # Save workbook
  openxlsx2::wb_save(wb, file = file_name)
}


prepare_freq_for_excel <- function(
  freq_df,
  show_genpop = FALSE,
  decimals = 1,
  drop_zero = FALSE,
  wt = NULL
) {
  # --- Extract metadata from the freq_df object ---

  # Get names of grouping variables from the attribute (e.g., Party, Gender)
  group_names <- attr(freq_df, "group_names")
  # Set to empty character vector if NULL
  if (is.null(group_names)) {
    group_names <- character(0)
  }
  # Ensure they are character strings
  group_names <- as.character(group_names)

  # get the main variable name from the attribute
  variable_name <- attr(freq_df, "variable_name")
  # Unwrap quosure or symbols to character
  variable_name <- as.character(rlang::quo_squash(variable_name))

  # --- Format percentage and count columns for Excel output ---

  # make pct col in freq_df a percentage string
  freq_df$pct <- round(freq_df$pct, digits = decimals + 2)
  # make_percent(freq_df$pct, decimals = decimals)
  # round the n column in freq_df to the nearest whole number
  freq_df$n <- round(freq_df$n)

  # --- If there are NO grouping variables, format as simple two-column output ---

  if (length(group_names) == 0) {
    # No groups: just select variable, pct, and n; rename columns
    wide <- freq_df[, c(variable_name, "pct", "n")]
    # rename the pct and n columns to include "Total" suffix
    names(wide)[2:3] <- c("pct", "n")
  } else {
    # --- If there ARE grouping variables, pivot the data to wide format ---

    # pivot: Create columns for each group x stat (e.g., Percent_White, N_White, etc.)
    wide <- freq_df %>%
      tidyr::pivot_wider(
        # specify the columns to pivot
        names_from = tidyselect::all_of(group_names),
        # specify what to put in the new columns
        values_from = c(pct, n)
      )

    # Sort columns nicely using your helper
    sorted_cols <- sort_cols(wide, group_names, variable_name, freq_df)

    # put the column names back together as a vector with "_" separating each part
    reordered <- do.call(paste, c(sorted_cols, sep = "_"))
    # reorder the columns using the reorderd vector of col names
    wide <- wide[c(variable_name, reordered)]

    # reorder the columns in the sorted data
    sorted_cols <- sorted_cols[, c(group_names, "pct_n")]
    # put the col names back together as a vector with the "_" separating each part
    fixed_cols <- do.call(paste, c(sorted_cols, sep = "_"))

    # rename the columns in data using fixed_cols
    names(wide) <- c(variable_name, fixed_cols)
  }

  # Add general population frequencies if requested
  if (show_genpop && length(group_names) > 0) {
    # get the original data set but remove the groups
    data <- dplyr::ungroup(attr(freq_df, "dataset"))

    wt <- names(data)[ncol(data)]

    # # get the frequencies of the general population
    genpop <- get_freqs(data, {{ variable_name }}, wt = {{ wt }})

    # convert pct to a percent
    genpop$pct <- round(genpop$pct, digits = decimals + 2)
    # make_percent(genpop$pct, decimals = decimals)
    # round the number of respondents
    genpop$n <- round(genpop$n)
    # rename the last two columns
    names(genpop)[names(genpop) %in% c("n", "pct")] <- c(
      "General Population_n",
      "General Population_pct"
    )
    # reorder the columns
    genpop <- genpop[c("General Population_pct", "General Population_n")]

    wide <- dplyr::bind_cols(wide, genpop)
  }

  wide
}


create_hierarchical_headers <- function(
  wb,
  sheet,
  wide_data,
  row_start,
  group_names = NULL,
  group_labels = NULL,
  add_comments = TRUE,
  delim = "_"
) {
  # Define replacements for common abbreviations
  replacements <- c("pct" = "Percent", "n" = "N")

  header_cols <- colnames(wide_data)[-1] # Exclude first column (variable names)

  if (length(header_cols) == 0) {
    return(list(wb = wb, rows_used = 0))
  }

  # Split headers by delimiter
  split_headers <- strsplit(header_cols, delim, fixed = TRUE)
  max_levels <- max(lengths(split_headers))

  # Create matrix for hierarchical headers
  header_matrix <- matrix("", nrow = max_levels, ncol = length(header_cols) + 1)
  header_matrix[max_levels, 1] <- colnames(wide_data)[1] # Variable name in bottom row

  # Fill in the header matrix
  for (i in seq_along(split_headers)) {
    parts <- split_headers[[i]]
    for (j in seq_along(parts)) {
      part <- parts[j]
      # Apply replacements if they exist
      if (part %in% names(replacements)) {
        part <- replacements[part]
      }
      header_matrix[j, i + 1] <- part
    }
  }

  # Write headers to workbook
  for (row in 1:max_levels) {
    wb <- wb %>%
      openxlsx2::wb_add_data(
        sheet = sheet,
        x = matrix(header_matrix[row, ], nrow = 1),
        start_row = row_start + row - 1,
        start_col = 1,
        col_names = FALSE
      )
  }

  # Style headers
  wb <- wb %>%
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(
        rows = row_start:(row_start + max_levels - 1),
        cols = 1:ncol(wide_data)
      ),
      bold = TRUE
    ) %>%
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(
        rows = row_start:(row_start + max_levels - 1),
        cols = 1:ncol(wide_data)
      ),
      color = openxlsx2::wb_color("#E6E6FA")
    ) %>%
    openxlsx2::wb_add_cell_style(
      dims = openxlsx2::wb_dims(
        rows = row_start:(row_start + max_levels - 1),
        cols = 1:ncol(wide_data)
      ),
      horizontal = "center",
      vertical = "center"
    )

  # Create spanners by merging cells with same values
  for (level in 1:(max_levels - 1)) {
    # Don't merge the bottom level
    current_row <- row_start + level - 1
    i <- 2 # Start from column 2 (skip variable name column)

    while (i <= ncol(header_matrix)) {
      if (header_matrix[level, i] != "") {
        # Find consecutive cells with same value
        start_col <- i
        current_value <- header_matrix[level, i]

        while (
          i <= ncol(header_matrix) && header_matrix[level, i] == current_value
        ) {
          i <- i + 1
        }
        end_col <- i - 1

        # Merge if more than one cell
        if (end_col > start_col) {
          wb <- wb %>%
            openxlsx2::wb_merge_cells(
              sheet = sheet,
              dims = openxlsx2::wb_dims(
                rows = current_row,
                cols = start_col:end_col
              )
            )
        }
      } else {
        i <- i + 1
      }
    }
  }

  # Add comments if requested
  if (add_comments && !is.null(group_names) && !is.null(group_labels)) {
    # Add ONE comment per grouping level (only to first relevant cell)
    for (level in 1:min(length(group_names), max_levels - 1)) {
      group_var <- group_names[level]
      group_label <- if (!is.null(group_labels[[group_var]])) {
        group_labels[[group_var]]
      } else {
        group_var
      }

      # Find first non-empty cell at this level
      current_row <- row_start + level - 1
      first_col_idx <- which(header_matrix[level, ] != "")

      if (length(first_col_idx) > 0) {
        first_col <- first_col_idx[1]
        wb <- wb %>%
          openxlsx2::wb_add_comment(
            sheet = sheet,
            dims = openxlsx2::wb_dims(rows = current_row, cols = first_col),
            comment = paste0("Grouping variable: ", group_label),
            author = "Info"
          )
      }
    }
  }

  return(list(wb = wb, rows_used = max_levels))
}
