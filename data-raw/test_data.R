# import data set
test_data <- haven::read_sav("data-raw/test_data.sav") %>%
  dplyr::rename(edu = edu_f) %>%
  dplyr::mutate(
    edu_f = case_match_fct(
      edu,
      c(1:2) ~ "High School or Less",
      3 ~ "Some College",
      4 ~ "Bachelor's Degree",
      5 ~ "Graduate Degree"
    )
  ) %>%
  labelled::set_variable_labels(wts = "Survey weighting variable")

usethis::use_data(test_data, overwrite = TRUE)
