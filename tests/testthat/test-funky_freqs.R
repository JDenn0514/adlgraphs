
# check with no grouping variables ---------------------------------------

testthat::test_that("check data is accurate with weights", {
  df <- test_data %>% funky_freqs(top, wt = wts)

  # check variable
  top <- df$top
  exp_top <- factor(
    c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree"),
    levels = c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree")
  ) %>% 
    structure(
      label = "An ideal society requires some groups to be on top and others to be on the bottom"
    )
  testthat::expect_equal(top, exp_top)

  # check n
  n <- df$n
  exp_n <- c(25.117, 86.107, 74.984, 58.907) %>% 
    structure(label = "N")
  testthat::expect_equal(n, exp_n)

  # check conf.low
  pct <- df$pct
  exp_pct <- c(0.10247, 0.35129, 0.30591, 0.24033) %>% 
    structure(label = "Percent")
  testthat::expect_equal(pct, exp_pct)
})

testthat::test_that("check data is accurate with no weights", {
  df <- test_data %>% funky_freqs(top)

  # check n
  n <- df$n
  exp_n <- c(25, 85, 75, 65) %>% 
    structure(label = "N")
  testthat::expect_equal(n, exp_n)

  # check conf.low
  pct <- df$pct
  exp_pct <- c(0.1, 0.34, 0.3, 0.26) %>% 
    structure(label = "Percent")
  testthat::expect_equal(pct, exp_pct)
})

testthat::test_that("check attributes are correct", {
  out <- attributes(test_data %>% funky_freqs(top, wt = wts))

  # check the 
  names <- out$names
  exp_names <- c("top", "n", "pct")
  testthat::expect_equal(names, exp_names)

  # check the variable label
  variable_label <- out$variable_label
  exp_variable_label <- "An ideal society requires some groups to be on top and others to be on the bottom"
  testthat::expect_equal(variable_label, exp_variable_label)

  # check the variable name
  variable_name <- out$variable_name
  exp_variable_name <- substitute(top)
  testthat::expect_equal(variable_name, exp_variable_name)

  # chec the class
  class <- out$class
  exp_class <- c("adlgraphs_freqs", "tbl_df", "tbl", "data.frame")
  testthat::expect_equal(class, exp_class)

})

testthat::test_that("check a vector with only partial value labels", {
  test_data$x <- sample(letters[1:4], size = 250, replace = TRUE) %>% 
    structure(labels = stats::setNames(letters[1:3], c("A", "B", "C")))
  
  df <- funky_freqs(test_data, x)
  exp <- structure(
    letters[1:4], 
    labels = stats::setNames(letters[1:3], c("A", "B", "C"))
  )
  testthat::expect_equal(df$x, exp)
})

testthat::test_that("check that drop_zero removes empty rows", {
  df2 <- data.frame(
    id = 1:5,
    type = factor(c("a", "c", "a", NA, "a"), levels = c("a", "b", "c"))
  )
  out <- df2 %>% funky_freqs(type, drop_zero = TRUE)
  exp <- c(3,1) %>% structure(label = "N")
  expect_equal(out$n, exp)
})

# check with one grouping variables ---------------------------------------

testthat::test_that("check data is accurate with weights", {
  df <- test_data %>% funky_freqs(top, pid_f3, wt = wts)

  # check variable
  top <- df$top
  exp_top <- factor(
    rep(c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree"), 3),
    levels = c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree")
  ) %>% 
    structure(
      label = "An ideal society requires some groups to be on top and others to be on the bottom"
    )
  testthat::expect_equal(top, exp_top)

  pid_f3 <- df$pid_f3
  # check pid_f3
  exp_pid_f3 <- factor(
    c(rep("Democrat", 4), rep("Independent", 4), rep("Republican", 4)),
    levels = c("Democrat", "Independent", "Republican")
  ) %>% 
    structure(
      label = "Political Partisanship",
      transformation = "Converted 'pid_f3' into a factor based on its value labels"
    )

  # check n
  n <- df$n
  exp_n <- c(10.188, 32.049, 38.177, 32.722,  1.816,  5.107,  7.119,  5.743, 13.112, 48.951, 29.687, 20.443) %>% 
    structure(label = "N")
  testthat::expect_equal(n, exp_n)

  # check conf.low
  pct <- df$pct
  exp_pct <- c(0.09005, 0.28328, 0.33745, 0.28923, 0.09181, 0.25811, 0.35983, 0.29025, 0.11687, 0.43631, 0.26461, 0.18221) %>% 
    structure(label = "Percent")
  testthat::expect_equal(pct, exp_pct)
})

testthat::test_that("check data is accurate with no weights", {
  df <- test_data %>% funky_freqs(top, pid_f3)

  # check n
  n <- df$n
  exp_n <- c(9, 26, 33, 30, 6, 17, 17, 17, 10, 42, 25, 18) %>% 
    structure(label = "N")
  testthat::expect_equal(n, exp_n)

  # check conf.low
  pct <- df$pct
  exp_pct <- c(0.09184, 0.26531, 0.33673, 0.30612, 0.10526, 0.29825, 0.29825, 0.29825, 0.10526, 0.44211, 0.26316, 0.18947) %>% 
    structure(label = "Percent")
  testthat::expect_equal(pct, exp_pct)
})

testthat::test_that("check attributes are correct", {
  out <- attributes(test_data %>% funky_freqs(top, pid_f3, wt = wts))

  # check the 
  names <- out$names
  exp_names <- c("pid_f3", "top", "n", "pct")
  testthat::expect_equal(names, exp_names)

  # check the variable label
  variable_label <- out$variable_label
  exp_variable_label <- "An ideal society requires some groups to be on top and others to be on the bottom"
  testthat::expect_equal(variable_label, exp_variable_label)

  # check the variable name
  variable_name <- out$variable_name
  exp_variable_name <- substitute(top)
  testthat::expect_equal(variable_name, exp_variable_name)

  # check the group names
  group_name <- out$group_name
  exp_group_name <- "pid_f3"
  testthat::expect_equal(group_name, exp_group_name)

  # check the group labels
  group_labels <- out$group_labels
  exp_group_labels <- c(pid_f3 = "Political Partisanship")
  testthat::expect_equal(group_labels, exp_group_labels)


  # chec the class
  class <- out$class
  exp_class <- c("adlgraphs_freqs", "grouped_df", "tbl_df", "tbl", "data.frame")
  testthat::expect_equal(class, exp_class)

})


# check with two grouping variables --------------------------------------


testthat::test_that("check attributes are correct", {
  out <- attributes(test_data %>% funky_freqs(top, c(edu_f2, pid_f3), wt = wts))

  # check the 
  names <- out$names
  exp_names <- c("edu_f2", "pid_f3", "top", "n", "pct")
  testthat::expect_equal(names, exp_names)

  # check the variable label
  variable_label <- out$variable_label
  exp_variable_label <- "An ideal society requires some groups to be on top and others to be on the bottom"
  testthat::expect_equal(variable_label, exp_variable_label)

  # check the variable name
  variable_name <- out$variable_name
  exp_variable_name <- substitute(top)
  testthat::expect_equal(variable_name, exp_variable_name)

  # check the group names
  group_name <- out$group_name
  exp_group_name <- c("edu_f2", "pid_f3")
  testthat::expect_equal(group_name, exp_group_name)

  # check the group labels
  group_labels <- out$group_labels
  exp_group_labels <- c(edu_f2 = "What is the highest level of school you have completed or the highest degree you have received?", pid_f3 = "Political Partisanship")
  testthat::expect_equal(group_labels, exp_group_labels)


  # chec the class
  class <- out$class
  exp_class <- c("adlgraphs_freqs", "grouped_df", "tbl_df", "tbl", "data.frame")
  testthat::expect_equal(class, exp_class)

})



# check errors for inputs ------------------------------------------------

testthat::test_that("error when wt is not numeric", {
  testthat::expect_snapshot(
    funky_freqs(test_data, top, wt = pid_f3),
    error = TRUE
  )
})
