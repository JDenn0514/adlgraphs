
# check to make sure output is correct ----------------------
testthat::test_that("expect outputs to be equal for mean with just x", {

  # manually calculate the mean
  mean_x <- test_data %>%
    dplyr::summarise(
      # calculate the mean
      mean = mean(trad_n,  na.rm = TRUE),
      # calculate the standard deviation
      sd = sd(trad_n, na.rm = TRUE),
      # get the number of respondents
      n = dplyr::n()
    ) %>%
    dplyr::mutate(
      # calculate the standard error
      std.error = sd/sqrt(n),
      # calculate the lower CI
      conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
      # calculate the higher CI
      conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
      # round all of the numbers to the second decimal
      dplyr::across(dplyr::where(is.numeric), ~round(.x, 2))
    )

  # test without quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means(trad_n)
  )
  # test with quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means("trad_n")
  )
})

testthat::test_that("expect outputs to be equal for mean with just x and wts", {

  # manually calculate the mean
  mean_x <- test_data %>%
    dplyr::summarise(
      # calculate the mean
      mean = weighted.mean(trad_n, wts, na.rm = TRUE),
      # calculate the standard deviation
      sd = sd(trad_n, na.rm = TRUE),
      # get the number of respondents
      n = dplyr::n()
    ) %>%
    dplyr::mutate(
      # calculate the standard error
      std.error = sd/sqrt(n),
      # calculate the lower CI
      conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
      # calculate the higher CI
      conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
      # round all of the numbers to the second decimal
      dplyr::across(dplyr::where(is.numeric), ~round(.x, 2))
    )

  # test without quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means(trad_n, wt = wts)
  )
  # test with quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means("trad_n", wt = "wts")
  )
})

testthat::test_that("expect outputs to be equal for mean with a factor grouping variable", {

  # manually calculate the mean
  mean_x <- test_data %>%
    dplyr::group_by(edu_f) %>%
    dplyr::summarise(
      # calculate the mean
      mean = mean(trad_n,  na.rm = TRUE),
      # calculate the standard deviation
      sd = sd(trad_n, na.rm = TRUE),
      # get the number of respondents
      n = dplyr::n()
    ) %>%
    dplyr::mutate(
      # calculate the standard error
      std.error = sd/sqrt(n),
      # calculate the lower CI
      conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
      # calculate the higher CI
      conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
      # round all of the numbers to the second decimal
      dplyr::across(dplyr::where(is.numeric), ~round(.x, 2))
    )

  # test without quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means(trad_n, edu_f)
  )
  # test with quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means("trad_n", "edu_f")
  )

})

testthat::test_that("expect outputs to be equal for mean with a labelled grouping variable", {

  # manually calculate the mean
  mean_x <- test_data %>%
    dplyr::mutate(accept_isr = haven::as_factor(accept_isr)) %>%
    dplyr::group_by(accept_isr) %>%
    dplyr::summarise(
      # calculate the mean
      mean = mean(trad_n,  na.rm = TRUE),
      # calculate the standard deviation
      sd = sd(trad_n, na.rm = TRUE),
      # get the number of respondents
      n = dplyr::n()
    ) %>%
    dplyr::mutate(
      # calculate the standard error
      std.error = sd/sqrt(n),
      # calculate the lower CI
      conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
      # calculate the higher CI
      conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
      # round all of the numbers to the second decimal
      dplyr::across(dplyr::where(is.numeric), ~round(.x, 2))
    )

  # test without quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means(trad_n, accept_isr)
  )
  # test with quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means("trad_n", "accept_isr")
  )

})

testthat::test_that("expect outputs to be equal for mean with a factor grouping variable and wts", {

  # manually calculate the mean
  mean_x <- test_data %>%
    dplyr::group_by(edu_f) %>%
    dplyr::summarise(
      # calculate the mean
      mean = weighted.mean(trad_n, wts, na.rm = TRUE),
      # calculate the standard deviation
      sd = sd(trad_n, na.rm = TRUE),
      # get the number of respondents
      n = dplyr::n()
    ) %>%
    dplyr::mutate(
      # calculate the standard error
      std.error = sd/sqrt(n),
      # calculate the lower CI
      conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
      # calculate the higher CI
      conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
      # round all of the numbers to the second decimal
      dplyr::across(dplyr::where(is.numeric), ~round(.x, 2))
    )

  # test without quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means(trad_n, edu_f, wt = wts)
  )
  # test with quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means("trad_n", "edu_f", wt = "wts")
  )
})

testthat::test_that("expect outputs to be equal for mean with a labelled group variable and wts", {

  # manually calculate the mean
  mean_x <- test_data %>%
    dplyr::mutate(accept_isr = haven::as_factor(accept_isr)) %>%
    dplyr::group_by(accept_isr) %>%
    dplyr::summarise(
    # calculate the mean
    mean = weighted.mean(trad_n, wts, na.rm = TRUE),
    # calculate the standard deviation
    sd = sd(trad_n, na.rm = TRUE),
    # get the number of respondents
    n = dplyr::n()
  ) %>%
    dplyr::mutate(
      # calculate the standard error
      std.error = sd/sqrt(n),
      # calculate the lower CI
      conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
      # calculate the higher CI
      conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
      # round all of the numbers to the second decimal
      dplyr::across(dplyr::where(is.numeric), ~round(.x, 2))
    )

  # test without quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means(trad_n, accept_isr, wt = wts)
  )
  # test with quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means("trad_n", accept_isr, wt = "wts")
  )
})


# check for no errors -----------------------------
testthat::test_that("Check x does not return error when numeric", {

  testthat::expect_no_error(test_data %>% get_means(trad_n))
  testthat::expect_no_error(test_data %>% get_means("trad_n"))

  testthat::expect_no_error(test_data %>% get_means(accept_isr))
  testthat::expect_no_error(test_data %>% get_means("accept_isr"))

  testthat::expect_no_error(test_data %>% get_means(trad_n, accept_isr))
  testthat::expect_no_error(test_data %>% get_means("trad_n", "accept_isr"))

  testthat::expect_no_error(test_data %>% get_means(trad_n, accept_isr))
  testthat::expect_no_error(test_data %>% get_means("trad_n", "accept_isr"))


})


