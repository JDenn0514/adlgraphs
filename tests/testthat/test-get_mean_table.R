#
#
#
#
# # test with just x and no variable label ----------------------
#
# # set this as the title
# trad_n <- "trad_n"
#
# mean_just_x <- test_data %>%
#   mutate(trad_n_no_var = as.numeric(trad_n)) %>%
#   dplyr::summarise(
#     # calculate the mean
#     mean = mean(trad_n_no_var,  na.rm = TRUE),
#     # calculate the standard deviation
#     sd = sd(trad_n_no_var, na.rm = TRUE),
#     # get the number of respondents
#     n = dplyr::n()
#   ) %>%
#   dplyr::mutate(
#     # calculate the standard error
#     std.error = sd/sqrt(n),
#     # calculate the lower CI
#     conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#     # calculate the higher CI
#     conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#     # round all of the numbers to the second decimal
#     dplyr::across(where(is.numeric), ~round(.x, 2))
#   ) %>%
#   gt::gt() %>%
#   # rename the columns
#   gt::cols_label(
#     mean = "Mean",
#     n = "N",
#     std.error = "Standard Error",
#     conf.low = "Lower CI",
#     conf.high = "Higher CI"
#   ) %>%
#   # add a title to the table of the variable label
#   gt::tab_header(
#     title = glue::glue('Average Score of "{trad_n}"'),
#     subtitle = "The minimum value is 0 and the maximum value is 14"
#   )
#
# ### get gt table with just x, x has no variable or value labels
#
# testthat::test_that(
#   "get mean gt table without quotes",
#   {testthat::expect_equal(
#     mean_trad_just_x,
#     test_data %>% mutate(trad_n = as.numeric(trad_n)) %>% get_mean_table(trad_n)
#   )}
# )
#
# testthat::test_that(
#   "get mean gt table with quotes",
#   {testthat::expect_equal(
#     mean_trad_just_x,
#     test_data %>% mutate(trad_n = as.numeric(trad_n)) %>%  get_mean_table("trad_n")
#   )}
# )
#
#
#
#
# # test with just x with a variable label but no value labels --------------------
#
# trad_n_label <- labelled::var_label(test_data[["trad_n"]])
#
# mean_just_x_and_var_label <- test_data %>%
#   dplyr::summarise(
#     # calculate the mean
#     mean = mean(trad_n,  na.rm = TRUE),
#     # calculate the standard deviation
#     sd = sd(trad_n, na.rm = TRUE),
#     # get the number of respondents
#     n = n()
#   ) %>%
#   dplyr::mutate(
#     # calculate the standard error
#     std.error = sd/sqrt(n),
#     # calculate the lower CI
#     conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#     # calculate the higher CI
#     conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#     # round all of the numbers to the second decimal
#     across(where(is.numeric), ~round(.x, 2))
#   ) %>%
#   gt::gt() %>%
#   # rename the columns
#   gt::cols_label(
#     mean = "Mean",
#     n = "N",
#     std.error = "Standard Error",
#     conf.low = "Lower CI",
#     conf.high = "Higher CI"
#   ) %>%
#   # add a title to the table of the variable label
#   gt::tab_header(
#     title = glue::glue('Average Score of "{trad_n_label}"'),
#     subtitle = "The minimum value is 0 and the maximum value is 14"
#   )
#
# ### get mean gt table for x with a variable label but no value labels
#
# testthat::test_that(
#   "checking to make sure the subtitle comes out properly",
#   {testthat::expect_equal(
#     mean_just_x_and_var_label,
#     test_data %>% get_mean_table(trad_n)
#   )}
# )
#
# testthat::test_that(
#   "checking to make sure the subtitle comes out properly",
#   {testthat::expect_equal(
#     mean_just_x_and_var_label,
#     test_data %>% get_mean_table("trad_n")
#   )}
# )
#
#
#
#
# # test just x with no variable label but with value labels ----------------
#
# test_data$new_stick_together <- labelled::remove_var_label(test_data$stick_together)
#
# new_stick_label <- "new_stick_together"
#
# mean_just_x_and_val_labels <- test_data %>%
#   dplyr::summarise(
#     # calculate the mean
#     mean = mean(new_stick_together,  na.rm = TRUE),
#     # calculate the standard deviation
#     sd = sd(new_stick_together, na.rm = TRUE),
#     # get the number of respondents
#     n = n()
#   ) %>%
#   dplyr::mutate(
#     # calculate the standard error
#     std.error = sd/sqrt(n),
#     # calculate the lower CI
#     conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#     # calculate the higher CI
#     conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#     # round all of the numbers to the second decimal
#     across(where(is.numeric), ~round(.x, 2))
#   ) %>%
#   gt::gt() %>%
#   # rename the columns
#   gt::cols_label(
#     mean = "Mean",
#     n = "N",
#     std.error = "Standard Error",
#     conf.low = "Lower CI",
#     conf.high = "Higher CI"
#   ) %>%
#   # add a title to the table of the variable label
#   gt::tab_header(
#     title = glue::glue('Average Score of "{new_stick_label}"'),
#     subtitle = "1 = Mostly true and 4 = Mostly false"
#   )
#
# ### get mean gt table for x with no variable label but with value labels
#
# testthat::test_that(
#   "get mean gt table with no quotes",
#   {testthat::expect_equal(
#     mean_just_x_and_val_labels,
#     test_data %>% get_mean_table(new_stick_together)
#   )}
# )
#
# testthat::test_that(
#   "get mean gt table with quotes",
#   {testthat::expect_equal(
#     mean_just_x_and_val_labels,
#     test_data %>% get_mean_table("new_stick_together")
#   )}
# )
#
# test_data <- test_data %>% select(-new_stick_together)
#
# # test with just x with a variable label but no value labels --------------------
#
# stick_together_label <- labelled::var_label(test_data[["stick_together"]])
#
# mean_just_x_and_var_label_and_val_label <- test_data %>%
#   dplyr::summarise(
#     # calculate the mean
#     mean = mean(stick_together,  na.rm = TRUE),
#     # calculate the standard deviation
#     sd = sd(stick_together, na.rm = TRUE),
#     # get the number of respondents
#     n = n()
#   ) %>%
#   dplyr::mutate(
#     # calculate the standard error
#     std.error = sd/sqrt(n),
#     # calculate the lower CI
#     conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#     # calculate the higher CI
#     conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#     # round all of the numbers to the second decimal
#     across(where(is.numeric), ~round(.x, 2))
#   ) %>%
#   gt::gt() %>%
#   # rename the columns
#   gt::cols_label(
#     mean = "Mean",
#     n = "N",
#     std.error = "Standard Error",
#     conf.low = "Lower CI",
#     conf.high = "Higher CI"
#   ) %>%
#   # add a title to the table of the variable label
#   gt::tab_header(
#     title = glue::glue('Average Score of "{stick_together_label}"'),
#     subtitle = "1 = Mostly true and 4 = Mostly false"
#   )
#
# ### get mean gt table for x with a variable label and value labels
#
# testthat::test_that(
#   "checking to make sure the subtitle comes out properly",
#   {testthat::expect_equal(
#     mean_just_x_and_var_label_and_val_label,
#     test_data %>% get_mean_table(stick_together)
#   )}
# )
#
# testthat::test_that(
#   "checking to make sure the subtitle comes out properly",
#   {testthat::expect_equal(
#     mean_just_x_and_var_label_and_val_label,
#     test_data %>% get_mean_table("stick_together")
#   )}
# )
#
#
#
# # tests with group -----------------------------------------------
#
# group_variable_label <- "edu_f"
# x_variable_label <- "trad_n"
#
# # calculate the mean for the general population
# genpop_df <- test_data %>%
#   dplyr::summarise(
#     # calculate the mean
#     mean = mean(trad_n,  na.rm = TRUE),
#     # calculate the standard deviation
#     sd = sd(trad_n, na.rm = TRUE),
#     # get the number of respondents
#     n = dplyr::n()
#   ) %>%
#   dplyr::mutate(
#     # calculate the standard error
#     std.error = sd/sqrt(n),
#     # calculate the lower CI
#     conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#     # calculate the higher CI
#     conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#     # round all of the numbers to the second decimal
#     across(where(is.numeric), ~round(.x, 2)),
#     # add a new variable using the group as the name
#     edu_f = "General Population"
#   )
#
# # calculate the mean for the group variable
# group_df <- test_data %>%
#   # group the data with the group variable
#   dplyr::group_by(edu_f) %>%
#   dplyr::summarise(
#     # calculate the mean
#     mean = mean(trad_n,  na.rm = TRUE),
#     # calculate the standard deviation
#     sd = sd(trad_n, na.rm = TRUE),
#     # get the number of respondents
#     n = n()
#   ) %>%
#   dplyr::mutate(
#     # calculate the standard error
#     std.error = sd/sqrt(n),
#     # calculate the lower CI
#     conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#     # calculate the higher CI
#     conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#     # round all of the numbers to the second decimal
#     dplyr::across(where(is.numeric), ~round(.x, 2)),
#   )
#
# # combine the two tables and make it a
# dplyr::bind_rows(genpop_df, group_df) %>%
#   # remove the standard deviation
#   dplyr::select(-sd) %>%
#   # move n to the end of the data
#   dplyr::relocate(n, .after = conf.high) %>%
#   # moev std.error after conf.high
#   dplyr::relocate(std.error, .after = conf.high) %>%
#   # make a gt object and set the group variable to be used as row labels
#   # placed in the table stub
#   gt::gt(rowname_col = "edu_f") %>%
#   # create a row group from a collection of rows
#   gt::tab_row_group(
#     # specify the row group label
#     label = group_variable_label,
#     # specify which rows, since genpop will always be on top this should
#     # rows 2 through group_length
#     rows = c(2:6)
#   ) %>%
#   # reorder the group order
#   gt::row_group_order(c(NA, group_variable_label)) %>%
#   # rename the columns
#   gt::cols_label(
#     mean = "Mean",
#     n = "N",
#     std.error = "Standard Error",
#     conf.low = "Lower CI",
#     conf.high = "Higher CI"
#   ) %>%
#   # add a title to the table of the variable label
#   gt::tab_header(
#     title = glue::glue("Average Score of {x_variable_label}"),
#     subtitle = "The minimum value is 0 and the maximum value is 14"
#   )
#
#
#
#
#
#
#
#
# test_data %>%
#   mutate(edu_n = as.numeric(edu)) %>%
#   dplyr::mutate(edu_n = haven::as_factor(edu_n)) %>%
#   # group the data with the group variable
#   dplyr::group_by(edu_n) %>%
#   dplyr::summarise(
#     # calculate the mean
#     mean = mean(trad_n,  na.rm = TRUE),
#     # calculate the standard deviation
#     sd = sd(trad_n, na.rm = TRUE),
#     # get the number of respondents
#     n = n()
#   )
#
#
