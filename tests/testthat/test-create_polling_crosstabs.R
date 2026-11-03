make_crosstab_df <- function() {
  set.seed(123)
  n <- 300
  df <- data.frame(
    gender = sample(c("Male", "Female"), n, replace = TRUE),
    party = sample(c("Dem", "Rep", "Ind"), n, replace = TRUE),
    approve = sample(c("Approve", "Disapprove"), n, replace = TRUE),
    direction = sample(c("Right direction", "Wrong track"), n, replace = TRUE),
    weight = stats::runif(n, 0.5, 2)
  )
  attr(df$gender, "label") <- "Gender"
  attr(df$party, "label") <- "Party"
  attr(df$approve, "label") <- "Job Approval"
  attr(df$direction, "label") <- "Country Direction"
  df
}

run_crosstabs <- function(file, progress) {
  create_polling_crosstabs(
    data = make_crosstab_df(),
    row_vars = c("approve", "direction"),
    subgroup_vars = c("gender", "party"),
    wt_var = "weight",
    min_n = 50,
    file_name = file,
    sheet_name = "Results",
    summary_string = "Test Poll",
    progress = progress
  )
}

test_that("progress messages report each banner column and the save", {
  file <- withr::local_tempfile(fileext = ".xlsx")
  msgs <- testthat::capture_messages(run_crosstabs(file, progress = TRUE))

  # Overall + 2 gender + 3 party columns
  expect_true(any(grepl("column 1 of 6 \\(Overall\\)", msgs)))
  expect_true(any(grepl("column 6 of 6 \\(party = Rep\\)", msgs)))
  expect_true(any(grepl("saving workbook", msgs)))
  expect_true(any(grepl("\\d+:\\d{2} elapsed", msgs)))
})

test_that("progress = FALSE silences progress messages", {
  file <- withr::local_tempfile(fileext = ".xlsx")
  msgs <- testthat::capture_messages(run_crosstabs(file, progress = FALSE))

  expect_false(any(grepl("^Crosstabs:", msgs)))
  # The existing export confirmation is unchanged
  expect_true(any(grepl("successfully exported", msgs)))
})

test_that("progress does not change the returned data or the workbook", {
  file_on <- withr::local_tempfile(fileext = ".xlsx")
  file_off <- withr::local_tempfile(fileext = ".xlsx")

  res_on <- suppressMessages(run_crosstabs(file_on, progress = TRUE))
  res_off <- suppressMessages(run_crosstabs(file_off, progress = FALSE))
  # The attached workbook carries timestamps and random style names
  attr(res_on, "wb") <- NULL
  attr(res_off, "wb") <- NULL
  expect_identical(res_on, res_off)

  wb_on <- openxlsx2::wb_load(file_on)
  wb_off <- openxlsx2::wb_load(file_off)
  expect_identical(
    openxlsx2::wb_to_df(wb_on, col_names = FALSE),
    openxlsx2::wb_to_df(wb_off, col_names = FALSE)
  )
  expect_identical(
    wb_on$worksheets[[1]]$sheet_data$cc,
    wb_off$worksheets[[1]]$sheet_data$cc
  )
  expect_identical(wb_on$styles_mgr$styles, wb_off$styles_mgr$styles)
  expect_identical(
    wb_on$worksheets[[1]]$mergeCells,
    wb_off$worksheets[[1]]$mergeCells
  )
})

test_that("weighted_pct() gives the same numbers as get_freqs()", {
  set.seed(42)
  n <- 400
  df <- data.frame(
    chr = sample(c("Yes", "No", NA), n, replace = TRUE),
    fct = factor(
      sample(c("Low", "Mid", NA), n, replace = TRUE),
      levels = c("Low", "Mid", "High") # "High" is never chosen
    ),
    num = sample(c(1, 2, 3, NA), n, replace = TRUE),
    lab = haven::labelled(
      sample(c(1, 2, NA), n, replace = TRUE),
      labels = c(Agree = 1, Disagree = 2, Unsure = 3)
    ),
    wts = stats::runif(n, 0.2, 3)
  )
  df$wts[c(3, 17, 250)] <- NA

  # Subsets as in banner columns, including one that drops rows
  for (rows in list(seq_len(n), which(df$wts > 1))) {
    sub <- df[rows, ]
    for (var in c("chr", "fct", "num", "lab")) {
      fast <- suppressWarnings(weighted_pct(sub[[var]], sub$wts))
      ref <- suppressWarnings(
        get_freqs(sub, x = tidyselect::all_of(var), wt = wts)
      )
      # as.numeric() drops the "Percent" label get_freqs() attaches
      expect_identical(
        fast,
        stats::setNames(as.numeric(ref$pct), as.character(ref[[var]]))
      )
    }
  }
})

test_that("add_border_by_style() draws the same borders as wb_add_border()", {
  black <- openxlsx2::wb_color(hex = "FF000000")
  base_wb <- function() {
    wb <- openxlsx2::wb_workbook()
    wb <- openxlsx2::wb_add_worksheet(wb, "s")
    wb <- openxlsx2::wb_add_data(wb, "s", matrix(1:40, 5), col_names = FALSE)
    wb <- openxlsx2::wb_add_fill(
      wb,
      "s",
      dims = "A1:B5",
      color = openxlsx2::wb_color(hex = "FFD3D3D3")
    )
    wb <- openxlsx2::wb_add_numfmt(wb, "s", dims = "C1:H5", numfmt = "0%")
    openxlsx2::wb_add_border(
      wb,
      "s",
      dims = "D1:D5",
      left_border = "medium",
      left_color = black,
      top_border = NULL,
      bottom_border = NULL,
      right_border = NULL,
      update = TRUE
    )
  }

  # Resolve each cell to its xf with the border id swapped for the border
  # itself, so the two workbooks compare by what they draw, not by ids
  resolved <- function(wb) {
    ids <- openxlsx2::wb_get_cell_style(wb, "s", dims = "A1:H5")
    xfs <- wb$styles_mgr$styles$cellXfs[as.integer(ids) + 1]
    border_ids <- as.integer(sub('.*borderId="([0-9]+)".*', "\\1", xfs))
    stopifnot(!anyNA(border_ids))
    paste(
      sub('borderId="[0-9]+"', "", xfs),
      wb$styles_mgr$styles$borders[border_ids + 1]
    )
  }

  steps <- list(
    list(rows = 2, cols = 1:8, top_border = "medium", bottom_border = NULL),
    list(rows = 4, cols = 1:8, top_border = "dashed", bottom_border = "dashed"),
    list(rows = 1:5, cols = 2, top_border = NULL, bottom_border = NULL)
  )

  direct <- base_wb()
  cached <- base_wb()
  cache <- new.env()
  for (s in steps) {
    args <- list(
      top_border = s$top_border,
      top_color = black,
      bottom_border = s$bottom_border,
      bottom_color = black,
      left_border = NULL,
      right_border = if (identical(s$cols, 2)) "medium" else NULL,
      right_color = black,
      update = TRUE
    )
    direct <- do.call(
      openxlsx2::wb_add_border,
      c(
        list(direct, sheet = "s", dims = openxlsx2::wb_dims(s$rows, s$cols)),
        args
      )
    )
    cached <- do.call(
      add_border_by_style,
      c(
        list(
          cached,
          cache = cache,
          sheet = "s",
          cells = sheet_cells(s$rows, s$cols)
        ),
        args
      )
    )
  }

  expect_identical(resolved(cached), resolved(direct))
  expect_lt(
    length(cached$styles_mgr$styles$cellXfs),
    length(direct$styles_mgr$styles$cellXfs)
  )
})
