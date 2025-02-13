
# Labelled values ---------------------------------------------------------

#### NUMERIC VECTORS
testthat::test_that("all labels are preserved if drop_levels is FALSE", {
  s1 <- haven::labelled(rep(1, 3), c("A" = 1, "B" = 2, "C" = 3), label = "Variable label")
  exp <- factor(rep("A", 3), levels = c("A", "B", "C")) %>%
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "Variable label"
    )
    testthat::expect_equal(make_factor(s1, drop_levels = FALSE), exp)
})

testthat::test_that("Only labels in data are preserved if drop_levels is TRUE", {
  s1 <- haven::labelled(rep(1, 3), c("A" = 1, "B" = 2, "C" = 3), label = "Variable label")
  exp <- factor(rep("A", 3), levels = c("A")) %>%
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "Variable label"
    )
  testthat::expect_equal(make_factor(s1, drop_levels = TRUE), exp)
})


testthat::test_that("all labels are preserved when using a non-haven_labelled numeric vector", {
  #values <- stats::setNames(c(0, 1), c("A", "B"))
  s1 <- structure(c(0, 1, 0, 1), labels = stats::setNames(c(0, 1), c("A", "B")))
  exp <- factor(c("A", "B", "A", "B"), levels = c("A", "B")) %>%
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "s1"
    )
  testthat::expect_equal(make_factor(s1), exp)
})

#### CHARACTER VECTORS WITH VALUE LABELS
testthat::test_that("character labelled converts to factor preserves all attributes", {
  s1 <- haven::labelled(c("M", "M", "F"), c(Male = "M", Female = "F"), "Gender")
  exp <- factor(c("Male", "Male", "Female"), levels = c("Male", "Female")) %>%
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "Gender"
    )
    testthat::expect_equal(make_factor(s1), exp)
})

testthat::test_that("character labelled converts to factor and uses name as label attribute", {
  s1 <- haven::labelled(c("M", "M", "F"), c(Male = "M", Female = "F"))
  exp <- factor(c("Male", "Male", "Female"), levels = c("Male", "Female")) %>%
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "s1"
    )
    testthat::expect_equal(make_factor(s1), exp)
})

testthat::test_that("all labels are preserved when using a non-haven_labelled numeric vector and uses name as label attribute", {
  #values <- stats::setNames(c(0, 1), c("A", "B"))
  s1 <- structure(c("A", "B", "A", "B"), labels = stats::setNames(c("A", "B"), c("Letter A", "Letter B")))
  exp <- factor(c("Letter A", "Letter B", "Letter A", "Letter B"), levels = c("Letter A", "Letter B")) %>%
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "s1"
    )
  testthat::expect_equal(make_factor(s1), exp)
})

testthat::test_that("all labels are preserved when using a non-haven_labelled numeric vector and uses name as label attribute", {
  #values <- stats::setNames(c(0, 1), c("A", "B"))
  s1 <- structure(c("A", "B", "A", "B"), labels = stats::setNames(c("A", "B"), c("Letter A", "Letter B")), label = "Alphabet")
  exp <- factor(c("Letter A", "Letter B", "Letter A", "Letter B"), levels = c("Letter A", "Letter B")) %>%
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "Alphabet"
    )
  testthat::expect_equal(make_factor(s1), exp)
})

#### CHARACTER VECTORS WITHOUT VALUE LABELS

testthat::test_that("convert to factor, add transformation and label attributes", {
  s1 <- letters
  exp <- structure(
    factor(letters),
    transformation = "Updated 's1' from a character vector to a factor",
    label = "s1"
  )
  testthat::expect_equal(make_factor(s1), exp)
})

testthat::test_that("convert to factor, add transformation and keep label attributes", {
  s1 <- letters %>% structure(label = "Alphabet")
  exp <- structure(
    factor(letters),
    transformation = "Updated 's1' from a character vector to a factor",
    label = "Alphabet"
  )
  testthat::expect_equal(make_factor(s1), exp)
})

testthat::test_that("should force s1 to a factor", {
  s1 <- c(0, 1, 2)
  testthat::expect_snapshot(make_factor(s1, force = TRUE))
})




# Check errors ----------------------------------------

# works
testthat::test_that("return error when some values are missing labels- numeric vector", {
  s1 <- haven::labelled(c(1, 4), c("Agree" = 1))
  testthat::expect_error(make_factor(s1), "Each value in `x` must have value labels")
})

testthat::test_that("return error when some values are missing labels- character vector", {
  s1 <- haven::labelled(c("A", "B"), c("Agree" = "A"))
  testthat::expect_error(make_factor(s1), "Each value in `x` must have value labels")
})











