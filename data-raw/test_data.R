
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
    edu_f2 = case_match_fct(
      edu,
      c(1, 2, 3) ~  "No College Degree",
      c(4, 5) ~ "At Least a Bachelor's Degree"
    ),
    resp_id = dplyr::row_number() %>%
      structure(
        label = "Unique ID for each Respondent",
        note = "Just the row number"
      ),
    pid_f3 = make_factor(pid_f3),

    ### social dominance orientation
    # reverse code them so that higher number means agreement with negative statement
    across(
      c(top:inferior),
      num_rev,
      .names = "{col}_rev"
    ),
    dominate_flip = haven::labelled(
      dominate,
      # we need to change the values to reflect the new variable label
      labels = c(
        "Strongly disagree" = 1,
        "Somewhat disagree" = 2,
        "Somewhat agree" = 3,
        "Strongly agree" = 4
      ),
      # we need to change the label to reflect the flipped valance
      label = "One group should dominate in society"
    ) %>%
      # add a note about the data transformation
      structure(
        transformation = "Flipped the valance of `dominate` so it is now negative by updating the label and value labels accordingly."
      ),
    # flip the valance of deserving and update the labels
    deserving_flip = haven::labelled(
      deserving,
      # we need to change the values to reflect the new variable label
      labels = c(
        "Strongly disagree" = 1,
        "Somewhat disagree" = 2,
        "Somewhat agree" = 3,
        "Strongly agree" = 4
      ),
      # we need to change the label to reflect the flipped valance
      label = "Groups at the bottom are not just as deserving as groups at the top"
    ) %>%
      # add a note about the data transformation
      structure(
        transformation = "Flipped the valance of `deserving` so it is now negative by updating the label and value labels accordingly."
      ),
    # calculate the sdo sum scores
    sdo_sum = rowSums(across(c(top_rev:inferior_rev, dominate_flip:deserving_flip))) %>%
      structure(
        label = "Social Dominance Orientation",
        transformation = "Simple sum of `top_rev`, `inferior_rev`, `dominate_flip`, `deserving_flip`."
      ),
    # make it an average score
    sdo_avg = sdo_sum / 4 %>%
      structure(
        label = "Social Dominance Orientation",
        transformation = "Divided `sdo_sum` by four to get the average score."
      ),
    # political participation
    pol_part_sum = rowSums(across(pol_part_rally:pol_part_attended)) %>%
      structure(
        label = "Political Participation",
        transformation = "Simple sum of `pol_part_rally`, `pol_part_worked`, `pol_part_contact`, `pol_part_money`, `pol_part_social`, `pol_part_attended`"
      ),

    across(
      c(pol_part_rally:pol_part_attended),
      ~structure(
        .x,
        question_preface = "In the last twelve months, which of the following have you done? Select all that apply "
      )
    ),

    ### conspiracy theories
    # reverse code them so that high number means more agreement
    across(
      c(controlled:big_events),
      num_rev,
      .names = "{col}_rev"
    ),
    # american conspiracy thinking scale
    acts_sum = rowSums(across(c(controlled_rev:big_events_rev))) %>%
      structure(
        label = "Conspiracy Theory Belief",
        transformation = "Simple sum of `controlled_rev`, `small_rev`, `run_rev`, `big_events_rev`.",
        note = "Based on 'American Conspiracy Thinking Scale'."
      ),
    # make the average score
    acts_avg = acts_sum / 4 %>%
      structure(
        label = "Social Dominance Orientation",
        transformation = "Divided `acts_sum` by four to get the average score."
      ),
    wts = structure(wts, label = "Survey weighting variable")
  ) %>%
  dplyr::relocate(resp_id)

usethis::use_data(test_data, overwrite = TRUE)




