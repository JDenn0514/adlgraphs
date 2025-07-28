
# create fake data
test_data <- tibble::tibble(
  x = c("A", "A", "B", "B", "B", "C", NA),
  group = c("G1", "G1", "G1", "G2", "G2", "G2", "G2"),
  wts = c(1, 2, 1, 1, 1, 1, 1)
)


# check to make sure output is correct ----------------------
testthat::test_that("get_freqs returns correct unweighted frequencies", {
  out <- get_freqs(test_data, x, na.rm = TRUE)
  testthat::expect_s3_class(out, "adlgraphs_freqs")
  testthat::expect_named(out, c("x", "n", "pct"))
  testthat::expect_equal(sum(out$n), 6)  # one NA removed
  testthat::expect_equal(round(sum(out$pct), 3), 1)
})

testthat::test_that("get_freqs returns correct weighted frequencies", {
  out <- get_freqs(test_data, x, wt = wts, na.rm = TRUE)
  testthat::expect_equal(out$n[out$x == "A"], 3)  # 1 + 2
  testthat::expect_equal(out$n[out$x == "B"], 3)  # 1 + 1
  testthat::expect_equal(round(sum(out$pct), 3), 1)
})

testthat::test_that("get_freqs handles grouping", {
  out <- get_freqs(test_data, x, group = group, wt = wts, na.rm = TRUE)
  testthat::expect_true(all(c("x", "group", "n", "pct") %in% names(out)))
  testthat::expect_equal(attr(out, "group_names"), "group")
  testthat::expect_s3_class(out, "adlgraphs_freqs")
})

testthat::test_that("get_freqs accepts strings for variables", {
  out1 <- get_freqs(test_data, "x", group = "group", wt = "wts", na.rm = TRUE)
  out2 <- get_freqs(test_data, x, group = group, wt = wts, na.rm = TRUE)

  # Check the data is equal
  testthat::expect_equal(dplyr::select(out1, -n, -pct), dplyr::select(out2, -n, -pct))
  testthat::expect_equal(out1$n, out2$n)
  testthat::expect_equal(out1$pct, out2$pct)

  # Compare attributes by value, not type
  testthat::expect_equal(as.character(attr(out1, "variable_label")),
               as.character(attr(out2, "variable_label")))
  testthat::expect_equal(as.character(attr(out1, "variable_name")),
               as.character(attr(out2, "variable_name")))
})


testthat::test_that("get_freqs drops zero counts if drop_zero = TRUE", {
  # Add a level with no observations
  test_data2 <- test_data %>% mutate(x = factor(x, levels = c("A", "B", "C", "D")))
  out <- get_freqs(test_data2, x, wt = wts, drop_zero = TRUE, na.rm = TRUE)
  testthat::expect_false("D" %in% out$x)
})

testthat::test_that("get_freqs throws error for non-numeric weights", {
  bad_data <- test_data %>% mutate(wts = as.character(wts))
  testthat::expect_error(get_freqs(bad_data, x, wt = wts), regexp = "must be a numeric variable")
})

