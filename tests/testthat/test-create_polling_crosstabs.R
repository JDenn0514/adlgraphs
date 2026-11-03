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
