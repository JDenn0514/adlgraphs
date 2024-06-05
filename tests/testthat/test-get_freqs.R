
# check to make sure output is correct ----------------------
testthat::test_that("expect outputs to be equal for freqs with just x", {

  # manually calculate the frequencies
  freq_x <- test_data %>%
    tidyr::drop_na(top) %>%
    dplyr::count(top) %>%
    dplyr::mutate(pct = prop.table(n)) %>%
    structure(class = c("adlgraphs_freqs", "tbl_df", "tbl", "data.frame"))


  # test without quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs(top)
  )
  # test with quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs("top")
  )
})

testthat::test_that("expect outputs to be equal for freqs with just x and wts", {

  # manually calculate the mean
  freq_x <- test_data %>%
    tidyr::drop_na(top) %>%
    dplyr::count(top, wt = wts) %>%
    dplyr::mutate(pct = prop.table(n),
                  n = round(n, 1)) %>%
    structure(class = c("adlgraphs_freqs", "tbl_df", "tbl", "data.frame"))

  # test without quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs(top, wt = wts)
  )
  # test with quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs("top", wt = "wts")
  )
})

testthat::test_that("expect outputs to be equal for freqs with a factor grouping variable", {

  # manually calculate the mean
  freq_x <- test_data %>%
    dplyr::group_by(edu_f) %>%
    tidyr::drop_na(top) %>%
    dplyr::count(top) %>%
    dplyr::mutate(pct = prop.table(n)) %>%
    structure(class = c("adlgraphs_freqs", "grouped_df", "tbl_df", "tbl", "data.frame"))

  # test without quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs(top, edu_f)
  )
  # test with quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs("top", "edu_f")
  )

})

testthat::test_that("expect outputs to be equal for freqs with a factor grouping variable and cross_tab = TRUE", {

  # manually calculate the mean
  freq_x <- test_data %>%
    dplyr::group_by(edu_f) %>%
    tidyr::drop_na(top) %>%
    dplyr::count(top) %>%
    dplyr::mutate(
      pct = prop.table(n),
      n = round(n, 1),
      pct = make_percent(pct),
      pct_lab = glue("{pct} (n = {n})"),
    ) %>%
    dplyr::select(-c(pct, n)) %>%
    tidyr::pivot_wider(
      names_from = edu_f,
      values_from = pct_lab
    ) %>%
    dplyr::arrange(top) %>%
    structure(class = c("adlgraphs_freqs", "tbl_df", "tbl", "data.frame"))

  # test without quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs(top, edu_f, cross_tab = TRUE)
  )
  # test with quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs("top", "edu_f", cross_tab = TRUE)
  )
})

testthat::test_that("expect outputs to be equal for freqs with a labelled grouping variable", {

  # manually calculate the mean
  freq_x <- test_data %>%
    # convert edu to a factor
    mutate(edu = haven::as_factor(edu)) %>%
    dplyr::group_by(edu) %>%
    tidyr::drop_na(top) %>%
    dplyr::count(top) %>%
    dplyr::mutate(pct = prop.table(n)) %>%
    structure(class = c("adlgraphs_freqs", "grouped_df", "tbl_df", "tbl", "data.frame"))

  # test without quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs(top, edu)
  )
  # test with quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs("top", "edu")
  )

})

testthat::test_that("expect outputs to be equal for mean with a labelled group variable and wts and cross_tab = TRUE", {

  # manually calculate the mean
  freq_x <- test_data %>%
    # convert edu to a factor
    mutate(edu = haven::as_factor(edu)) %>%
    dplyr::group_by(edu) %>%
    tidyr::drop_na(top) %>%
    dplyr::count(top) %>%
    dplyr::mutate(
      pct = prop.table(n),
      n = round(n, 1),
      pct = make_percent(pct),
      pct_lab = glue("{pct} (n = {n})"),
    ) %>%
    dplyr::select(-c(pct, n)) %>%
    tidyr::pivot_wider(
      names_from = edu,
      values_from = pct_lab
    ) %>%
    dplyr::arrange(top) %>%
    structure(class = c("adlgraphs_freqs", "tbl_df", "tbl", "data.frame"))

  # test without quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs(top, edu, cross_tab = TRUE)
  )
  # test with quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs("top", edu, cross_tab = TRUE)
  )
})

testthat::test_that("expect outputs to be equal for freqs with a factor grouping variable and wts", {

  # manually calculate the mean
  freq_x <- test_data %>%
    dplyr::group_by(edu_f) %>%
    tidyr::drop_na(top) %>%
    dplyr::count(top, wt = wts) %>%
    dplyr::mutate(pct = prop.table(n),
                  n = round(n, 1)) %>%
    structure(class = c("adlgraphs_freqs", "grouped_df", "tbl_df", "tbl", "data.frame"))

  # test without quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs(top, edu_f, wt = wts)
  )
  # test with quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs("top", "edu_f", wt = "wts")
  )
})

testthat::test_that("expect outputs to be equal for mean with a labelled group variable and wts", {

  # manually calculate the mean
  freq_x <- test_data %>%
    # convert edu to a factor
    mutate(edu = haven::as_factor(edu)) %>%
    dplyr::group_by(edu) %>%
    tidyr::drop_na(top) %>%
    dplyr::count(top, wt = wts) %>%
    dplyr::mutate(pct = prop.table(n),
                  n = round(n, 1)) %>%
    structure(class = c("adlgraphs_freqs", "grouped_df", "tbl_df", "tbl", "data.frame"))

  # test without quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs(top, edu, wt = wts)
  )
  # test with quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs("top", edu, wt = "wts")
  )
})

testthat::test_that("expect outputs to be equal for freqs with a factor grouping variable and wts and cross_tab = TRUE", {

  # manually calculate the mean
  freq_x <- test_data %>%
    dplyr::group_by(edu_f) %>%
    tidyr::drop_na(top) %>%
    dplyr::count(top, wt = wts) %>%
    dplyr::mutate(
      pct = prop.table(n),
      n = round(n, 1),
      pct = make_percent(pct),
      pct_lab = glue("{pct} (n = {n})"),
    ) %>%
    dplyr::select(-c(pct, n)) %>%
    tidyr::pivot_wider(
      names_from = edu_f,
      values_from = pct_lab
    ) %>%
    dplyr::arrange(top) %>%
    structure(class = c("adlgraphs_freqs", "tbl_df", "tbl", "data.frame"))

  # test without quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs(top, edu_f, wt = wts, cross_tab = TRUE)
  )
  # test with quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs("top", "edu_f", wt = "wts", cross_tab = TRUE)
  )
})

testthat::test_that("expect outputs to be equal for mean with a labelled group variable and wts and cross_tab = TRUE", {

  # manually calculate the mean
  freq_x <- test_data %>%
    # convert edu to a factor
    mutate(edu = haven::as_factor(edu)) %>%
    dplyr::group_by(edu) %>%
    tidyr::drop_na(top) %>%
    dplyr::count(top, wt = wts) %>%
    dplyr::mutate(
      pct = prop.table(n),
      n = round(n, 1),
      pct = make_percent(pct),
      pct_lab = glue("{pct} (n = {n})"),
    ) %>%
    dplyr::select(-c(pct, n)) %>%
    tidyr::pivot_wider(
      names_from = edu,
      values_from = pct_lab
    ) %>%
    dplyr::arrange(top) %>%
    structure(class = c("adlgraphs_freqs", "tbl_df", "tbl", "data.frame"))

  # test without quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs(top, edu, wt = wts, cross_tab = TRUE)
  )
  # test with quotes
  testthat::expect_equal(
    freq_x,
    test_data %>% get_freqs("top", edu, wt = "wts", cross_tab = TRUE)
  )
})





