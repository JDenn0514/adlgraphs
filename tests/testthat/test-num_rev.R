
testthat::test_that("check a vector with value labels but no variable label", {
  s1 <- structure(
    c(1:4), 
    labels = stats::setNames(c(1:4), c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree"))
  )
  exp <- structure(
    c(4, 3, 2, 1), 
    labels = stats::setNames(c(1:4), c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")),
    label = "s1",
    transformation = "Reversing \'s1\' while maintaining correct value labels"
  )
  testthat::expect_equal(num_rev(s1), exp)
})

testthat::test_that("check a vector with no value labels but with variable label", {
  s1 <- structure(
    c(1:4), 
    # labels = stats::setNames(c(1:4), c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree")),
    label = "Variable label"
  )
  exp <- structure(
    c(4, 3, 2, 1), 
    # labels = stats::setNames(c(1:4), c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")),
    label = "Variable label",
    transformation = "Reversing \'s1\'"
  )
  testthat::expect_equal(num_rev(s1), exp)
})

testthat::test_that("check a vector with variable and value labels", {
  s1 <- structure(
    c(1:4), 
    labels = stats::setNames(c(1:4), c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree")),
    label = "Variable label"
  )
  exp <- structure(
    c(4, 3, 2, 1), 
    labels = stats::setNames(c(1:4), c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")),
    label = "Variable label",
    transformation = "Reversing \'s1\' while maintaining correct value labels"
  )
  testthat::expect_equal(num_rev(s1), exp)
})

# check errors ----------------
testthat::test_that("check for a factor input", {
  testthat::expect_snapshot(
    num_rev(test_data$pid_f3),
    error = TRUE
  )
})

testthat::test_that("return error when some values are missing labels- numeric vector", {
  s1 <- structure(c(1, 4), labels = c("Agree" = 1))
  testthat::expect_error(num_rev(s1), "Each value in `x` must have value labels")
})





