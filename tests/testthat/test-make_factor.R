
# Labelled values ---------------------------------------------------------

#### NUMERIC VECTORS
test_that("all labels are preserved", {
  s1 <- haven::labelled(rep(1, 3), c("A" = 1, "B" = 2, "C" = 3), label = "Variable label")
  exp <- factor(rep("A", 3), levels = c("A", "B", "C")) %>%
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "Variable label"
    )
  expect_equal(make_factor(s1), exp)
})

test_that("all labels are preserved when using a non-haven_labelled numeric vector", {
  #values <- stats::setNames(c(0, 1), c("A", "B"))
  s1 <- structure(c(0, 1, 0, 1), labels = stats::setNames(c(0, 1), c("A", "B")))
  exp <- factor(c("A", "B", "A", "B"), levels = c("A", "B")) %>%
    structure(transformation = "Converted 's1' into a factor based on its value labels")
  expect_equal(make_factor(s1), exp)
})

#### CHARACTER VECTORS
test_that("character labelled converts to factor", {
  s1 <- haven::labelled(c("M", "M", "F"), c(Male = "M", Female = "F"))
  exp <- factor(c("Male", "Male", "Female"), levels = c("Male", "Female")) %>%
    structure(
      transformation = "Converted 's1' into a factor based on its value labels"
    )
  expect_equal(make_factor(s1), exp)
})

test_that("all labels are preserved when using a non-haven_labelled numeric vector", {
  #values <- stats::setNames(c(0, 1), c("A", "B"))
  s1 <- structure(c("A", "B", "A", "B"), labels = stats::setNames(c("A", "B"), c("Letter A", "Letter B")))
  exp <- factor(c("Letter A", "Letter B", "Letter A", "Letter B"), levels = c("Letter A", "Letter B")) %>%
    structure(transformation = "Converted 's1' into a factor based on its value labels")
  expect_equal(make_factor(s1), exp)
})





# Check errors ----------------------------------------

# works
test_that("return error when some values are missing labels- numeric vector", {
  s1 <- haven::labelled(c(1, 4), c("Agree" = 1))
  expect_error(make_factor(s1), "Each value in `x` must have value labels")
})

test_that("return error when some values are missing labels- character vector", {
  s1 <- haven::labelled(c("A", "B"), c("Agree" = "A"))
  expect_error(make_factor(s1), "Each value in `x` must have value labels")
})

test_that("return error when there aren't any value labels- numeric vector", {
  s1 <- c(0, 1, 2)
  expect_error(make_factor(s1), "The vector provided in `x` does not have value labels")
})

test_that("return error when there aren't any value labels- character vector", {
  s1 <- letters
  expect_error(make_factor(s1), "The vector provided in `x` does not have value labels")
})








