test_data %>% get_diffs(trad_n, pid_f3, edu_f2, wt = wts)

testthat::test_that("check diffs is accurate", {
  diffs <- test_data %>% get_diffs(trad_n, pid_f3, edu_f2, wt = wts)
  diffs <- diffs$diffs
  exp <- c(1.955, 0.099, 0.637, 0.305) %>% 
    structure(label = "ADL Index")

  testthat::expect_equal(diffs, exp)
})
