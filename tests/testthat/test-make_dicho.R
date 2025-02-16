
testthat::test_that("Check a character vector", {
  char <- c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree")
  exp <- factor(c("Agree", "Agree", "Disagree", "Disagree"), levels = c("Agree", "Disagree")) %>% 
    structure(transformation = "Converting 'char' to a dichotomous factor with 'Agree' as the reference level")
  testthat::expect_equal(make_dicho(char), exp)
})


testthat::test_that("check a factor", {
  char <- factor(
    c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree"), 
    levels = c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree")
  )
  exp <- factor(c("Agree", "Agree", "Disagree", "Disagree"), levels = c("Agree", "Disagree")) %>% 
    structure(transformation = "Converting 'char' to a dichotomous factor with 'Agree' as the reference level")
  testthat::expect_equal(make_dicho(char), exp)
})

testthat::test_that("check a labelled numeric vector", {
  s1 <- structure(
    c(1:4), 
    labels = stats::setNames(c(1:4), c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree")))
  exp <- factor(c("Agree", "Agree", "Disagree", "Disagree"), levels = c("Agree", "Disagree")) %>% 
    structure(transformation = "Converting 's1' to a dichotomous factor with 'Agree' as the reference level")
  testthat::expect_equal(make_dicho(s1), exp)
})

testthat::test_that("error when x is not numeric", {
  testthat::expect_snapshot(
    make_dicho(c(0:4)),
    error = TRUE
  )
})

testthat::test_that("Check flip_levels = TRUE", {
  char <- c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree")
  exp <- factor(c("Agree", "Agree", "Disagree", "Disagree"), levels = c("Disagree", "Agree")) %>% 
    structure(transformation = "Converting 'char' to a dichotomous factor and reordering the factor levels so that 'Disagree' is the reference level")
  testthat::expect_equal(make_dicho(char, flip_levels = TRUE), exp)
})

