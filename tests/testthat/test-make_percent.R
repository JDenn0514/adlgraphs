testthat::test_that("convert to a percentage", {
  numbers <- c(0.05, 0.00078, 0.1, 0.2598)
  testthat::expect_equal(
    paste0(round(numbers*100, 2), "%") %>% structure(transformation = "Added a `%` symbol to `numbers`"),
    make_percent(numbers)
  )
})

testthat::test_that("convert to a percentage with no decimals", {
  numbers <- c(0.05, 0.00078, 0.1, 0.2598)
  testthat::expect_equal(
    paste0(round(numbers*100), "%") %>% structure(transformation = "Added a `%` symbol to `numbers`"),
    make_percent(numbers, 0)
  )
})

testthat::test_that("Keep attributes", {
  numbers <- c(0.05, 0.00078, 0.1, 0.2598) %>% 
    structure(label = "Variable label")
  exp <- paste0(round(numbers * 100, 2), "%") %>% 
    structure(
      label = "Variable label",
      transformation = "Added a `%` symbol to `numbers`"
    )
  testthat::expect_equal(make_percent(numbers), exp)
})

testthat::test_that("error when not numeric", {
  testthat::expect_error(make_percent(letters))
})


