
testthat::test_that("LHS can match multiple values", {
  out <- case_match_fct(1, 1:2 ~ "x")
  exp <- factor("x", levels = "x") %>% 
    structure(transformation = "Recoded '1' as a factor and set the levels based on their order.\nThe data transformation is as follows:\nWhat was '1:2' has become 'x'")
  testthat::expect_equal(out, exp)
})


testthat::test_that("LHS can match special values", {
  out <- case_match_fct(NA, NA ~ "x")
  exp <- factor("x", levels = "x") %>% 
    structure(transformation = "Recoded 'NA' as a factor and set the levels based on their order.\nThe data transformation is as follows:\nWhat was 'NA' has become 'x'")
  testthat::expect_equal(out, exp)
})

testthat::test_that("`NULL` values in `...` are dropped", {
  testthat::expect_error(
    case_match_fct(1:2, 1 ~ "a", NULL, 2 ~ "b", NULL)
  )
})

testthat::test_that("requires at least one condition", {
  testthat::expect_snapshot(error = TRUE, {
    case_match_fct(1)
  })
  testthat::expect_snapshot(error = TRUE, {
    case_match_fct(1, NULL)
  })
})

testthat::test_that("check that .default works properly", {

  species <- c("Human", "Hutt", "Yoda's Species", "Gungan", "Droid", "Wookiee", "Ewok", "Aleena") %>% 
    structure(label = "Star Wars Species")
  species <- case_match_fct(
    species,
    "Human" ~ "Humanoid",
    "Droid" ~ "Robot",
    c("Wookiee", "Ewok") ~ "Hairy",
    .default = "Other"
  )
  exp <- factor(
    c("Humanoid", "Other", "Other", "Other", "Robot", "Hairy", "Hairy", "Other"),
    levels = c("Humanoid", "Robot", "Hairy", "Other")
  ) %>% 
    structure(
      label = "Star Wars Species",
      transformation = "Recoded 'species' as a factor and set the levels based on their order.\nThe data transformation is as follows:\nWhat was 'Human' has become 'Humanoid'\nWhat was 'Droid' has become 'Robot'\nWhat was 'c(\"Wookiee\", \"Ewok\")' has become 'Hairy'\nEverything else has become 'Other'"
    )
  
  testthat::expect_equal(species, exp)
})





