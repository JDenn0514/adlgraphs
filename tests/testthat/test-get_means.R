
# check to make sure output is correct ----------------------
testthat::test_that("expect outputs to be equal for mean with x, no groups and no wts", {

  mean_x <- data.frame(
    mean = 4.22,
    sd = 3.872,
    n = 250,
    conf.low = 3.738,
    conf.high = 4.702
  )

  # add a variable for the n variable
  attr(mean_x$n, "label") <- "N"
  # add a variable label for the mean variable
  attr(mean_x$mean, "label") <- "Mean"
  # add a variable label for the mean variable
  attr(mean_x$sd, "label") <- "SD"
  # add a variable label for the mean variable
  attr(mean_x$conf.low, "label") <- "Low CI"
  # add a variable label for the mean variable
  attr(mean_x$conf.high, "label") <- "High CI"

  class(mean_x) <- c("adlgraphs_means", "tbl_df", "tbl", "data.frame")

  attr(mean_x, "variable_label") <- "ADL Index"
  attr(mean_x, "variable_name") <- "trad_n" 

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

testthat::test_that("expect outputs to be equal for mean with x, no groups and no wts", {

  mean_x <- data.frame(
    mean = 4.22,
    sd = 3.872,
    n = 250,
    conf.low = 3.738,
    conf.high = 4.702
  )

  # add a variable for the n variable
  attr(mean_x$n, "label") <- "N"
  # add a variable label for the mean variable
  attr(mean_x$mean, "label") <- "Mean"
  # add a variable label for the mean variable
  attr(mean_x$sd, "label") <- "SD"
  # add a variable label for the mean variable
  attr(mean_x$conf.low, "label") <- "Low CI"
  # add a variable label for the mean variable
  attr(mean_x$conf.high, "label") <- "High CI"

  class(mean_x) <- c("adlgraphs_means", "tbl_df", "tbl", "data.frame")

  attr(mean_x, "variable_label") <- "trad_n"
  attr(mean_x, "variable_name") <- "trad_n"

  test_data$trad_n <- as.numeric(test_data$trad_n)

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

testthat::test_that("expect outputs to be equal when ci is 0.9", {

  mean_x <- data.frame(
    mean = 3.973,
    sd = 3.757,
    n = 245.115,
    conf.low = 3.577,
    conf.high = 4.369
  )

  # add a variable for the n variable
  attr(mean_x$n, "label") <- "N"
  # add a variable label for the mean variable
  attr(mean_x$mean, "label") <- "Mean"
  # add a variable label for the mean variable
  attr(mean_x$sd, "label") <- "SD"
  # add a variable label for the mean variable
  attr(mean_x$conf.low, "label") <- "Low CI"
  # add a variable label for the mean variable
  attr(mean_x$conf.high, "label") <- "High CI"

  class(mean_x) <- c("adlgraphs_means", "tbl_df", "tbl", "data.frame")

  attr(mean_x, "variable_label") <- "ADL Index"
  attr(mean_x, "variable_name") <- "trad_n" 

  # test without quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means(trad_n, wt = wts, conf_level = 0.9)
  )
  # test with quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means("trad_n", wt = "wts", conf_level = 0.9)
  )
})


testthat::test_that("expect outputs to be equal when decimals is 2", {

  mean_x <- data.frame(
    mean = 3.97,
    sd = 3.76,
    n = 245.11,
    conf.low = 3.5,
    conf.high = 4.45
  )

  # add a variable for the n variable
  attr(mean_x$n, "label") <- "N"
  # add a variable label for the mean variable
  attr(mean_x$mean, "label") <- "Mean"
  # add a variable label for the mean variable
  attr(mean_x$sd, "label") <- "SD"
  # add a variable label for the mean variable
  attr(mean_x$conf.low, "label") <- "Low CI"
  # add a variable label for the mean variable
  attr(mean_x$conf.high, "label") <- "High CI"

  class(mean_x) <- c("adlgraphs_means", "tbl_df", "tbl", "data.frame")

  attr(mean_x, "variable_label") <- "ADL Index"
  attr(mean_x, "variable_name") <- "trad_n" 

  # test without quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means(trad_n, wt = wts, decimals = 2)
  )
  # test with quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means("trad_n", wt = "wts", decimals = 2)
  )
})

testthat::test_that("expect outputs to be equal for mean with x, no groups and wts", {

  mean_x <- data.frame(
    mean = 3.973,
    sd = 3.757,
    n = 245.115,
    conf.low = 3.501,
    conf.high = 4.446
  )

  # add a variable for the n variable
  attr(mean_x$n, "label") <- "N"
  # add a variable label for the mean variable
  attr(mean_x$mean, "label") <- "Mean"
  # add a variable label for the mean variable
  attr(mean_x$sd, "label") <- "SD"
  # add a variable label for the mean variable
  attr(mean_x$conf.low, "label") <- "Low CI"
  # add a variable label for the mean variable
  attr(mean_x$conf.high, "label") <- "High CI"

  class(mean_x) <- c("adlgraphs_means", "tbl_df", "tbl", "data.frame")

  attr(mean_x, "variable_label") <- "ADL Index"
  attr(mean_x, "variable_name") <- "trad_n" 

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

testthat::test_that("expect outputs to be equal for mean with x, one group and wts", {

  # manually calculate the mean
  mean_x <- test_data %>%
    group_by(edu_f2) %>% 
    dplyr::summarise(
      # calculate the weighted n
      n = sum(wts, na.rm = TRUE), 
      # calculate the mean (weighted sum / n)
      mean = sum(trad_n * wts, na.rm = TRUE) / n,
      # calculate the weighted sd
      sd = sqrt(sum(wts * (trad_n - mean)^2, na.rm = TRUE) / n),
      # remove the groups
      .groups = "drop"
    ) %>% 
    dplyr::mutate(
      # calculate std.error
      std.error = sd / sqrt(n),
      # calculate the confidence invtervals
      conf.low = mean - stats::qt(1 - ((1 - 0.95) / 2), n - 1) * std.error,
      conf.high = mean + stats::qt(1 - ((1 - 0.95) / 2), n - 1) * std.error,
      # convert all group variables to a factor
      dplyr::across(
        # run the function over the variables in group_names
        c(edu_f2),
        # convert to a factor, removing levels, forcing to factor, and keeping NA as NA
        ~ make_factor(.x, drop_levels = TRUE, force = TRUE, na.rm = TRUE)
      ),
      # round all numeric columns 
      dplyr::across(
        tidyselect::where(is.numeric),
        ~round(.x, 3)
      )
    ) %>% 
    # keep only relevant variables and reorder them
    dplyr::select(c(edu_f2, mean, sd, n, conf.low, conf.high))

  # add a variable for the n variable
  attr(mean_x$n, "label") <- "N"
  # add a variable label for the mean variable
  attr(mean_x$mean, "label") <- "Mean"
  # add a variable label for the mean variable
  attr(mean_x$sd, "label") <- "SD"
  # add a variable label for the mean variable
  attr(mean_x$conf.low, "label") <- "Low CI"
  # add a variable label for the mean variable
  attr(mean_x$conf.high, "label") <- "High CI"

  class(mean_x) <- c("adlgraphs_means", "tbl_df", "tbl", "data.frame")

  attr(mean_x, "group_names") <- c("edu_f2")
  attr(mean_x, "variable_label") <- "ADL Index"
  attr(mean_x, "variable_name") <- "trad_n" 

  # test without quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means(trad_n, c(edu_f2), wt = wts)
  )
  # test with quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means("trad_n", c("edu_f2"), wt = wts)
  )
})


testthat::test_that("expect outputs to be equal for mean with x, one group with NAs and wts", {

  test_data$pid_f3_NA <- ifelse(test_data$pid_f3 == "Independent", NA, paste(test_data$pid_f3))

  test_data <- test_data[c("trad_n", "pid_f3_NA", "wts")]
  
  # manually calculate the mean
  mean_x <- test_data %>%
    dplyr::group_by(pid_f3_NA) %>% 
    dplyr::summarise(
      # calculate the weighted n
      n = sum(wts, na.rm = TRUE), 
      # calculate the mean (weighted sum / n)
      mean = sum(trad_n * wts, na.rm = TRUE) / n,
      # calculate the weighted sd
      sd = sqrt(sum(wts * (trad_n - mean)^2, na.rm = TRUE) / n),
    ) %>% 
    dplyr::mutate(
      # calculate std.error
      std.error = sd / sqrt(n),
      # calculate the confidence invtervals
      conf.low = mean - stats::qt(1 - ((1 - 0.95) / 2), n - 1) * std.error,
      conf.high = mean + stats::qt(1 - ((1 - 0.95) / 2), n - 1) * std.error,
      # convert all group variables to a factor
      dplyr::across(
        # run the function over the variables in group_names
        c(pid_f3_NA),
        # convert to a factor, removing levels, forcing to factor, and keeping NA as NA
        ~ make_factor(.x, drop_levels = TRUE, force = TRUE, na.rm = TRUE)
      ),
      # round all numeric columns 
      dplyr::across(
        tidyselect::where(is.numeric),
        ~round(.x, 3)
      )
    )
    # keep only relevant variables and reorder them
  mean_x <- mean_x[c("pid_f3_NA", "mean", "sd", "n", "conf.low", "conf.high")]

  # add a variable for the n variable
  attr(mean_x$n, "label") <- "N"
  # add a variable label for the mean variable
  attr(mean_x$mean, "label") <- "Mean"
  # add a variable label for the mean variable
  attr(mean_x$sd, "label") <- "SD"
  # add a variable label for the mean variable
  attr(mean_x$conf.low, "label") <- "Low CI"
  # add a variable label for the mean variable
  attr(mean_x$conf.high, "label") <- "High CI"

  class(mean_x) <- c("adlgraphs_means", "tbl_df", "tbl", "data.frame")

  attr(mean_x, "group_names") <- c("pid_f3_NA")
  attr(mean_x, "variable_label") <- "ADL Index"
  attr(mean_x, "variable_name") <- "trad_n" 

  # test with NAs
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means(trad_n, c(pid_f3_NA), wt = wts, na.rm = FALSE)
  )
  
  # remove NAs from the group variables
  mean_x <- mean_x[stats::complete.cases(mean_x),]
  # test without NAs
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means(trad_n, c(pid_f3_NA), wt = wts)
  )

})

# check to make sure output is correct ----------------------
testthat::test_that("expect outputs to be equal for mean with x, two groups and wts", {

  # manually calculate the mean
  mean_x <- test_data %>%
    group_by(edu_f2, pid_f3) %>% 
    dplyr::summarise(
      # calculate the weighted n
      n = sum(wts, na.rm = TRUE), 
      # calculate the mean (weighted sum / n)
      mean = sum(trad_n * wts, na.rm = TRUE) / n,
      # calculate the weighted sd
      sd = sqrt(sum(wts * (trad_n - mean)^2, na.rm = TRUE) / n),
      # remove the groups
      .groups = "drop"
    ) %>% 
    dplyr::mutate(
      # calculate std.error
      std.error = sd / sqrt(n),
      # calculate the confidence invtervals
      conf.low = mean - stats::qt(1 - ((1 - 0.95) / 2), n - 1) * std.error,
      conf.high = mean + stats::qt(1 - ((1 - 0.95) / 2), n - 1) * std.error,
      # convert all group variables to a factor
      dplyr::across(
        # run the function over the variables in group_names
        c(edu_f2, pid_f3),
        # convert to a factor, removing levels, forcing to factor, and keeping NA as NA
        ~ make_factor(.x, drop_levels = TRUE, force = TRUE, na.rm = TRUE)
      ),
      # round all numeric columns 
      dplyr::across(
        tidyselect::where(is.numeric),
        ~round(.x, 3)
      )
    ) %>% 
    # keep only relevant variables and reorder them
    dplyr::select(c(edu_f2, pid_f3, mean, sd, n, conf.low, conf.high))

  # add a variable for the n variable
  attr(mean_x$n, "label") <- "N"
  # add a variable label for the mean variable
  attr(mean_x$mean, "label") <- "Mean"
  # add a variable label for the mean variable
  attr(mean_x$sd, "label") <- "SD"
  # add a variable label for the mean variable
  attr(mean_x$conf.low, "label") <- "Low CI"
  # add a variable label for the mean variable
  attr(mean_x$conf.high, "label") <- "High CI"

  class(mean_x) <- c("adlgraphs_means", "tbl_df", "tbl", "data.frame")

  attr(mean_x, "group_names") <- c("edu_f2", "pid_f3")
  attr(mean_x, "variable_label") <- "ADL Index"
  attr(mean_x, "variable_name") <- "trad_n" 

  # test without quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means(trad_n, c(edu_f2, pid_f3), wt = wts)
  )
  # test with quotes
  testthat::expect_equal(
    mean_x,
    test_data %>% get_means("trad_n", c("edu_f2", "pid_f3"), wt = wts)
  )
})



# check for no errors -----------------------------
testthat::test_that("Check x does not return error when group is numeric", {

  testthat::expect_no_error(test_data %>% get_means(trad_n))
  testthat::expect_no_error(test_data %>% get_means("trad_n"))

  testthat::expect_no_error(test_data %>% get_means(accept_isr))
  testthat::expect_no_error(test_data %>% get_means("accept_isr"))

  testthat::expect_no_error(test_data %>% get_means(trad_n, accept_isr))
  testthat::expect_no_error(test_data %>% get_means("trad_n", "accept_isr"))

  testthat::expect_no_error(test_data %>% get_means(trad_n, accept_isr))
  testthat::expect_no_error(test_data %>% get_means("trad_n", "accept_isr"))


})


# check for errors from inputs -------------------------------------------

testthat::test_that("error when x is not numeric", {
  testthat::expect_snapshot(
    get_means(test_data, pid_f3),
    error = TRUE
  )
})

testthat::test_that("error when wt is not numeric", {
  testthat::expect_snapshot(
    get_means(test_data, trad_n, wt = pid_f3),
    error = TRUE
  )
})
