testthat::test_that("check baseline with variable label", {
  s1 <- structure(
    c(1:4), 
    labels = stats::setNames(c(1:4), c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree")),
    label = "Variable label"
  )
  exp <- structure(
    c(1, 1, 0, 0),
    transformation = "Converting 's1' to a binary variable with 'Agree' = 1 and 'Disagree' = 0.",
    labels = stats::setNames(c(1, 0), c("Agree", "Disagree")),
    label = "Variable label"
  )     
  testthat::expect_equal(make_binary(s1), exp)
})

testthat::test_that("check if flip_values = TRUE", {
  s1 <- structure(
    c(1:4), 
    labels = stats::setNames(c(1:4), c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree")),
    label = "Variable label"
  )
  exp <- structure(
    c(0, 0, 1, 1),
    transformation = "Converting 's1' to a binary variable with 'Disagree' = 1 and 'Agree' = 0.",
    labels = stats::setNames(c(1, 0), c("Disagree", "Agree")),
    label = "Variable label"
  )     
  testthat::expect_equal(make_binary(s1, flip_values = TRUE), exp)
})

testthat::test_that("check without a variable label", {
  s1 <- structure(
    c(1:4), 
    labels = stats::setNames(c(1:4), c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree"))
  )
  exp <- structure(
    c(1, 1, 0, 0),
    transformation = "Converting 's1' to a binary variable with 'Agree' = 1 and 'Disagree' = 0.",
    labels = stats::setNames(c(1, 0), c("Agree", "Disagree")),
    label = "s1"
  )     
  testthat::expect_equal(make_binary(s1), exp)
})


