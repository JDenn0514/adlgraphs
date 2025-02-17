
# No grouping variables -----------------------------------------
testthat::test_that("check all responses are accurate", {
  # get the df
  df <- test_data %>% get_diffs(trad_n, pid_f3, wt = wts)
  # check diffs
  diffs <- df$diffs
  exp_diffs <- c(1.706, 0.176) %>% 
    structure(label = "Difference in means relative to Democrat")
  testthat::expect_equal(diffs, exp_diffs)

  # check n
  n <- df$n
  exp_n <- c(pid_f3Independent = 57, pid_f3Republican = 95) %>% 
    structure(label = "N")
  testthat::expect_equal(n, exp_n)

  # check conf.low
  conf.low <- df$conf.low
  exp_conf.low <- c(-0.077, -0.799) %>% 
    structure(label = "Low CI")
  testthat::expect_equal(conf.low, exp_conf.low)

  # check conf.low
  conf.high <- df$conf.high
  exp_conf.high <- c(3.490, 1.151) %>% 
    structure(label = "High CI")
  testthat::expect_equal(conf.high, exp_conf.high)

  # check p-value
  p_value <- df$p_value
  exp_p_value <- c(0.061, 0.722) %>% 
    structure(label = "P-Value")
  testthat::expect_equal(p_value, exp_p_value)

})

testthat::test_that("check that it updates the reference level with no group", {
  df <- test_data %>% get_diffs(trad_n, pid_f3, wt = wts, ref_level = "Independent")
  diffs <- df$diffs
  exp <- c(-1.706, -1.530) %>% 
    structure(label = "Difference in means relative to Independent")
  testthat::expect_equal(diffs, exp)
})

testthat::test_that("check that it works with no weights", {
  df <- test_data %>% get_diffs(trad_n, pid_f3)
  diffs <- df$diffs
  exp <- c(1.895, 0.274) %>% 
    structure(label = "Difference in means relative to Democrat")
  testthat::expect_equal(diffs, exp)
})

testthat::test_that("check with show_pct_change = TRUE", {
  df <- test_data %>% get_diffs(trad_n, pid_f3, wt = wts, show_pct_change = TRUE)
  pct_change <- df$pct_change
  exp <- c(0.45439, 0.04697) %>% 
    structure(label = "Percent change from Democrat")
  testthat::expect_equal(pct_change, exp)

  # adjust decimals
  df <- test_data %>% get_diffs(trad_n, pid_f3, wt = wts, show_pct_change = TRUE, decimals = 1)
  pct_change <- df$pct_change
  exp <- c(0.454, 0.047) %>% 
    structure(label = "Percent change from Democrat")
  testthat::expect_equal(pct_change, exp)
})

testthat::test_that("check with show_means = TRUE", {
  df <- test_data %>% get_diffs(trad_n, pid_f3, wt = wts, show_means = TRUE)
  means <- df$mean
  exp <- c(3.755, 5.461, 3.931) %>% 
    structure(label = "Mean")
  testthat::expect_equal(means, exp)
})

testthat::test_that("check with show_means = TRUE", {
  df <- test_data %>% get_diffs(trad_n, pid_f3, wt = wts, show_means = TRUE)
  means <- df$mean
  exp <- c(3.755, 5.461, 3.931) %>% 
    structure(label = "Mean")
  testthat::expect_equal(means, exp)
})

testthat::test_that("check with no variable label", {
  df <- test_data %>% 
    dplyr::mutate(trad_n = as.numeric(trad_n)) %>% 
    get_diffs(trad_n, pid_f3, wt = wts, show_means = TRUE)
  exp <- substitute(trad_n)
  testthat::expect_equal(attr(df, "variable_label"), exp)
})


# Check with one grouping variable -------------------------

testthat::test_that("check that the results are accurate ", {
  # get the df
  df <- test_data %>% get_diffs(trad_n, pid_f3, edu_f2, wt = wts)
  # check diffs
  diffs <- df$diffs
  exp_diffs <- c(1.955, 0.099, 0.637, 0.305) %>% 
    structure(label = "Difference in means relative to Democrat")
  testthat::expect_equal(diffs, exp_diffs)

  # check n
  n <- df$n
  exp_n <- c(pid_f3Independent = 40, pid_f3Republican = 50,pid_f3Independent = 17, pid_f3Republican = 45) %>% 
    structure(label = "N")
  testthat::expect_equal(n, exp_n)

  # check conf.low
  conf.low <- df$conf.low
  exp_conf.low <- c(-0.237, -1.232, -2.894, -1.146) %>% 
    structure(label = "Low CI")
  testthat::expect_equal(conf.low, exp_conf.low)

  # check conf.low
  conf.high <- df$conf.high
  exp_conf.high <- c(4.146, 1.429, 4.168, 1.757) %>% 
    structure(label = "High CI")
  testthat::expect_equal(conf.high, exp_conf.high)

  # check p-value
  p_value <- df$p_value
  exp_p_value <- c(0.080, 0.883, 0.721, 0.677) %>% 
    structure(label = "P-Value")
  testthat::expect_equal(p_value, exp_p_value)
})


# check with two grouping variables ------------------------------

testthat::test_that("check that the results are accurate ", {
  # get the df
  df <- test_data %>% 
    dplyr::mutate(top_f2 = make_dicho(top)) %>% 
    get_diffs(trad_n, pid_f3, c(edu_f2, top_f2), wt = wts)
  # check diffs
  diffs <- df$diffs
  exp_diffs <- c(2.782, -0.657, 1.863, 0.303, 1.387,  0.086, -0.299, -0.450) %>% 
    structure(label = "Difference in means relative to Democrat")
  testthat::expect_equal(diffs, exp_diffs)

  # check n
  n <- df$n
  exp_n <- c(
    pid_f3Independent = 15, 
    pid_f3Republican = 28,
    pid_f3Independent = 25, 
    pid_f3Republican = 22,
    pid_f3Independent = 8, 
    pid_f3Republican = 24,
    pid_f3Independent = 9, 
    pid_f3Republican = 21
  ) %>% 
    structure(label = "N")
  testthat::expect_equal(n, exp_n)

  # check conf.low
  conf.low <- df$conf.low
  exp_conf.low <- c(-1.049, -2.624, -0.686, -1.474, -4.708, -2.492, -4.354, -2.159) %>% 
    structure(label = "Low CI")
  testthat::expect_equal(conf.low, exp_conf.low)

  # check conf.low
  conf.high <- df$conf.high
  exp_conf.high <- c( 6.614, 1.309, 4.413, 2.081, 7.483, 2.665, 3.757, 1.259) %>% 
    structure(label = "High CI")
  testthat::expect_equal(conf.high, exp_conf.high)

  # check p-value
  p_value <- df$p_value
  exp_p_value <- c(0.152, 0.507, 0.150, 0.735, 0.648, 0.946, 0.883, 0.601) %>% 
    structure(label = "P-Value")
  testthat::expect_equal(p_value, exp_p_value)
})

testthat::test_that("check results are same with dplyr::group_by()", {
  testthat::expect_equal(
    test_data %>% get_diffs(trad_n, pid_f3, edu_f2),
    test_data %>% dplyr::group_by(edu_f2) %>% get_diffs(trad_n, pid_f3)
  )

  testthat::expect_equal(
    test_data %>% get_diffs(trad_n, pid_f3, c(edu_f2, top)),
    test_data %>% dplyr::group_by(edu_f2, top) %>% get_diffs(trad_n, pid_f3)
  )

  testthat::expect_equal(
    test_data %>% get_diffs(trad_n, pid_f3, c(edu_f2, top)),
    test_data %>% dplyr::group_by(edu_f2) %>% get_diffs(trad_n, pid_f3, top)
  )
})


# check errors for inputs ------------------------------------------------

testthat::test_that("error when x is not numeric", {
  testthat::expect_snapshot(
    test_data %>% get_diffs(edu_f2, pid_f3),
    error = TRUE
  )
})

testthat::test_that("error when wt is not numeric", {
  testthat::expect_snapshot(
    test_data %>% get_diffs(trad_n, pid_f3, wt = edu_f2),
    error = TRUE
  )
})



