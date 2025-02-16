# no groups
testthat::test_that("Check error message when no group is provided", {
  testthat::expect_error(make_nested(test_data), "No grouping variables were provided. Must supply at least one.")
})


