testthat::test_that("row_means is equal to rowMeans", {

  test <- test_data %>% 
    dplyr::mutate(
      new = row_means(
        cols = c(top_rev:deserving_flip),
        label = "Social Dominance Orientation Average",
        na.rm = TRUE
      ),
      base = rowMeans(dplyr::pick(c(top_rev:deserving_flip)), na.rm = TRUE) %>% 
        structure(
          label = "Social Dominance Orientation Average",
          variables = c("top_rev", "inferior_rev", "dominate_flip", "deserving_flip"),
          transformation = "Took the average of top_rev, inferior_rev, dominate_flip, deserving_flip"
        )
    )
  
  testthat::expect_equal(test$new, test$base)

})

testthat::test_that("row_means is equal to rowMeans without label", {

  test <- test_data %>% 
    dplyr::mutate(
      new = row_means(
        cols = c(top_rev:deserving_flip),
        na.rm = TRUE
      ),
      base = rowMeans(dplyr::pick(c(top_rev:deserving_flip)), na.rm = TRUE) %>% 
        structure(
          variables = c("top_rev", "inferior_rev", "dominate_flip", "deserving_flip"),
          transformation = "Took the average of top_rev, inferior_rev, dominate_flip, deserving_flip"
        )
    )
  
  testthat::expect_equal(test$new, test$base)

})



