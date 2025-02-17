testthat::test_that("check it works properly", {
  top <- test_data$top
  out <- case_when_fct(
    top < 3 ~ "Agree",
    top > 2 ~ "Disagree"
  ) 
  exp <- make_dicho(top) %>% 
    structure(transformation = NULL, label = NULL)

  expect_equal(out, exp)

})





