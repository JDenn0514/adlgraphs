
# No grouping variables --------------------------------------------------

testthat::test_that("Check that it works with no weights", {
  df <- test_data %>% get_corr(x = trad_n, y = sdo_sum)
  # check the x column
  x <- df$x
  exp_x <- c("trad_n") %>% 
    structure(labels = c("ADL Index" = "trad_n"))
  class(exp_x) <- c("haven_labelled", "vctrs_vctr", "character")
  testthat::expect_equal(x, exp_x)

  # check the y column
  y <- df$y
  exp_y <- c("sdo_sum") %>% 
    structure(labels = c("Social Dominance Orientation" = "sdo_sum"))
  class(exp_y) <- c("haven_labelled", "vctrs_vctr", "character")
  testthat::expect_equal(y, exp_y)

  # check the correlation
  testthat::expect_equal(df$correlation, 0.23 %>% structure(label = "Correlation"))
  # check n
  testthat::expect_equal(df$n, 250 %>% structure(label = "N"))
  # check conf.low
  testthat::expect_equal(df$conf.low, 0.108 %>% structure(label = "Low CI"))
  # check conf.high
  testthat::expect_equal(df$conf.high, 0.351 %>% structure(label = "High CI"))
  # check p_value
  testthat::expect_equal(df$p_value, 0 %>% structure(label = "P-Value"))

  # check that works with and without quotes
  testthat::expect_equal(
    test_data %>% get_corr(x = "trad_n", y = "sdo_sum"),
    test_data %>% get_corr(x = trad_n, y = sdo_sum)
  )

  # check that works with test_data inside the call
  testthat::expect_equal(
    get_corr(test_data, x = "trad_n", y = "sdo_sum"),
    test_data %>% get_corr(x = trad_n, y = sdo_sum)
  )
})

testthat::test_that("Check that it works with weights", {
  df <- test_data %>% get_corr(x = trad_n, y = sdo_sum, wt = wts)
  # check the x column
  # check the correlation
  testthat::expect_equal(df$correlation, 0.257 %>% structure(label = "Correlation"))
  # check n
  testthat::expect_equal(df$n, 250 %>% structure(label = "N"))
  # check conf.low
  testthat::expect_equal(df$conf.low, 0.136 %>% structure(label = "Low CI"))
  # check conf.high
  testthat::expect_equal(df$conf.high, 0.378 %>% structure(label = "High CI"))
  # check p_value
  testthat::expect_equal(df$p_value, 0 %>% structure(label = "P-Value"))
})


# check with one group ---------------------------------------------------

testthat::test_that("Check that it works with weights", {
  df <- test_data %>% get_corr(x = trad_n, y = sdo_sum, edu_f2, wt = wts)
  # check the x column
  x <- df$x
  exp_x <- c("trad_n", "trad_n") %>% 
    structure(labels = c("ADL Index" = "trad_n"))
  class(exp_x) <- c("haven_labelled", "vctrs_vctr", "character")
  testthat::expect_equal(x, exp_x)

  # check the y column
  y <- df$y
  exp_y <- c("sdo_sum", "sdo_sum") %>% 
    structure(labels = c("Social Dominance Orientation" = "sdo_sum"))
  class(exp_y) <- c("haven_labelled", "vctrs_vctr", "character")
  testthat::expect_equal(y, exp_y)

  # check the correlation
  testthat::expect_equal(df$correlation, c(0.163, 0.425) %>% structure(label = "Correlation"))
  # check n
  testthat::expect_equal(df$n, c(142, 108) %>% structure(label = "N"))
  # check conf.low
  testthat::expect_equal(df$conf.low, c(-0.002, 0.25) %>% structure(label = "Low CI"))
  # check conf.high
  testthat::expect_equal(df$conf.high, c(0.328, 0.599) %>% structure(label = "High CI"))
  # check p_value
  testthat::expect_equal(df$p_value, c(0.052, 0) %>% structure(label = "P-Value"))

  # check that works with dplyr::group_by
  testthat::expect_equal(
    test_data %>% 
      dplyr::group_by(edu_f2) %>% 
      get_corr(x = trad_n, y = sdo_sum, wt = wts),
    test_data %>% get_corr(x = trad_n, y = sdo_sum, edu_f2, wt = wts)
  )

})

# check with two groups --------------------------------------------------

testthat::test_that("Check that it works with weights", {
  df <- test_data %>% get_corr(x = trad_n, y = sdo_sum, c(edu_f2, pid_f3), wt = wts)
  # check the x column
  x <- df$x
  exp_x <- c("trad_n", "trad_n", "trad_n", "trad_n", "trad_n", "trad_n") %>% 
    structure(labels = c("ADL Index" = "trad_n"))
  class(exp_x) <- c("haven_labelled", "vctrs_vctr", "character")
  testthat::expect_equal(x, exp_x)

  # check the y column
  y <- df$y
  exp_y <- c("sdo_sum", "sdo_sum", "sdo_sum", "sdo_sum", "sdo_sum", "sdo_sum") %>% 
    structure(labels = c("Social Dominance Orientation" = "sdo_sum"))
  class(exp_y) <- c("haven_labelled", "vctrs_vctr", "character")
  testthat::expect_equal(y, exp_y)

  # check the correlation
  testthat::expect_equal(df$correlation, c(0.429, 0.179, 0.002, 0.297, 0.321, 0.548) %>% structure(label = "Correlation"))
  # check n
  testthat::expect_equal(df$n, c(52, 40, 50, 46, 17, 45) %>% structure(label = "N"))
  # check conf.low
  testthat::expect_equal(df$conf.low, c(0.172, -0.144, -0.288,  0.007, -0.200,  0.291) %>% structure(label = "Low CI"))
  # check conf.high
  testthat::expect_equal(df$conf.high, c(0.685, 0.502, 0.292, 0.587, 0.842, 0.805) %>% structure(label = "High CI"))
  # check p_value
  testthat::expect_equal(df$p_value, c(0.002, 0.270, 0.990, 0.045, 0.209, 0.000) %>% structure(label = "P-Value"))

  # check that works with dplyr::group_by
  testthat::expect_equal(
    test_data %>% 
      dplyr::group_by(edu_f2) %>% 
      get_corr(x = trad_n, y = sdo_sum, wt = wts),
    test_data %>% get_corr(x = trad_n, y = sdo_sum, edu_f2, wt = wts)
  )

  testthat::expect_equal(
    test_data %>% 
      dplyr::group_by(edu_f2, pid_f3) %>% 
      get_corr(x = trad_n, y = sdo_sum, wt = wts),
    test_data %>% get_corr(x = trad_n, y = sdo_sum, c(edu_f2, pid_f3), wt = wts)
  )

  testthat::expect_equal(
    test_data %>% 
      dplyr::group_by(edu_f2) %>% 
      get_corr(x = trad_n, y = sdo_sum, pid_f3, wt = wts),
    test_data %>% get_corr(x = trad_n, y = sdo_sum, c(edu_f2, pid_f3), wt = wts)
  )

})


# wtd_corr and stdz checks ---------------------------------------------------------
testthat::test_that("check wtd_corr without wts", {
  df <- test_data %>% wtd_corr(x = trad_n, y = sdo_sum)
  # check the x column
  x <- df$x
  exp_x <- c("trad_n") %>% 
    structure(labels = c("ADL Index" = "trad_n"))
  class(exp_x) <- c("haven_labelled", "vctrs_vctr", "character")
  testthat::expect_equal(x, exp_x)

  # check the y column
  y <- df$y
  exp_y <- c("sdo_sum") %>% 
    structure(labels = c("Social Dominance Orientation" = "sdo_sum"))
  class(exp_y) <- c("haven_labelled", "vctrs_vctr", "character")
  testthat::expect_equal(y, exp_y)

  # check the correlation
  testthat::expect_equal(df$correlation, 0.23 %>% structure(label = "Correlation"))
  # check n
  testthat::expect_equal(df$n, 250 %>% structure(label = "N"))
  # check conf.low
  testthat::expect_equal(df$conf.low, 0.108 %>% structure(label = "Low CI"))
  # check conf.high
  testthat::expect_equal(df$conf.high, 0.351 %>% structure(label = "High CI"))
  # check p_value
  testthat::expect_equal(df$p_value, 0 %>% structure(label = "P-Value"))
})


# test for errors for incorrect inputs  ----------------------------------------

testthat::test_that("error when x is not numeric", {
  testthat::expect_snapshot(
    get_corr(test_data, edu_f2, trad_n),
    error = TRUE
  )
})

testthat::test_that("error when y is not numeric", {
  testthat::expect_snapshot(
    get_corr(test_data, trad_n, edu_f2),
    error = TRUE
  )
})


testthat::test_that("error when y is not numeric", {
  testthat::expect_snapshot(
    get_corr(test_data, trad_n, sdo_sum, wt = pid_f3),
    error = TRUE
  )
})


