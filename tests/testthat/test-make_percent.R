testthat::test_that("convert to a percentage", {
  numbers <- c(0.05, 0.00078, 0.1, 0.2598)
  testthat::expect_equal(
    paste0(round(numbers*100, 2), "%"),
    make_percent(numbers)
  )
})

testthat::test_that("convert to a percentage with one decimal", {
  numbers <- c(0.05, 0.00078, 0.1, 0.2598)
  testthat::expect_equal(
    paste0(round(numbers*100), "%"),
    make_percent(numbers, 0)
  )
})


