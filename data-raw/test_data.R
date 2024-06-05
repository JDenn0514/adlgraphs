test_data <- pol_pos[sample(nrow(pol_pos), 250), ]

test_data <- test_data %>%
  select(wts, edu, edu_f2, pid_f3, top:big_events, accept_hamas:dislike_jews, stick_together:wall_street, acts_avg, sdo_avg, trad_n)

yar <- labelled::lookfor(test_data)

haven::write_sav(test_data, here::here("data-raw", "test_data.sav"))

rm(test_data)

# import data set
test_data <- haven::read_sav("data-raw/test_data.sav") %>%
  dplyr::mutate(
    edu_f = case_match_fct(
      edu,
      c(1:2) ~ "High School or Less",
      3 ~ "Some College",
      4 ~ "Bachelor's Degree",
      5 ~ "Graduate Degree"
    ),
    edu_f2 = haven::as_factor(edu_f2),
    resp_id = dplyr::row_number() %>%
      structure(
        label = "Unique ID for each Respondent",
        note = "Just the"
      )
  ) %>%
  labelled::set_variable_labels(wts = "Survey weighting variable") %>%
  relocate(resp_id)

usethis::use_data(test_data, overwrite = TRUE)
