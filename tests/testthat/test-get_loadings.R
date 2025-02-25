# Check get_loadings.default -------------------------------------------------------------------
testthat::test_that("Check output with sort = TRUE", {
  out <- test_data %>% 
    dplyr::select(inferior:controlled) %>% 
    psych::fa(nfactors = 1, rotate = "oblimin", fm = "pa") %>% 
    get_loadings(sort = TRUE, threshold = 0)

  # check the names of the columns in the output
  exp_var_names <- c("variables", "PA1", "communality", "uniqueness")
  testthat::expect_equal(names(out), exp_var_names)

  # check the variables column
  exp_var <- c("inferior", "controlled", "deserving", "special", "harder", "dominate")
  attr(exp_var, "label") <- "Variable Name"
  testthat::expect_equal(out$variables, exp_var)

  # check the loadings
  exp_load <- c(0.733,  0.327, -0.297,  0.239,  0.173, -0.172)
  attr(exp_load, "label") <- "Factor 1"
  testthat::expect_equal(out$PA1, exp_load)

  # check the loadings
  exp_comm <- c(0.537, 0.107, 0.088, 0.057, 0.030, 0.029)
  attr(exp_comm, "label") <- "Communality"
  testthat::expect_equal(out$communality, exp_comm)

  # check the loadings
  exp_uni <- c(0.463, 0.893, 0.912, 0.943, 0.970, 0.971)
  attr(exp_uni, "label") <- "Uniqueness"
  testthat::expect_equal(out$uniqueness, exp_uni)

})

testthat::test_that("check with sort = FALSE", {
  out <- test_data %>% 
    dplyr::select(inferior:controlled) %>% 
    psych::fa(nfactors = 1, rotate = "oblimin", fm = "pa") %>% 
    get_loadings(sort = FALSE, threshold = 0)

  # check the variables column
  exp_var <- c("inferior", "dominate", "deserving", "special", "harder", "controlled")
  attr(exp_var, "label") <- "Variable Name"
  testthat::expect_equal(out$variables, exp_var)

  # check the loadings
  exp_load <- c(0.733, -0.172, -0.297, 0.239, 0.173, 0.327)
  attr(exp_load, "label") <- "Factor 1"
  testthat::expect_equal(out$PA1, exp_load)

})

testthat::test_that("check with the baseline threshold of 0.4", {
  out <- test_data %>% 
    dplyr::select(inferior:controlled) %>% 
    psych::fa(nfactors = 1, rotate = "oblimin", fm = "pa") %>% 
    get_loadings()

  # check the loadings
  exp_load <- c(0.733, NA, NA, NA, NA, NA)
  attr(exp_load, "label") <- "Factor 1"
  testthat::expect_equal(out$PA1, exp_load)
})

testthat::test_that("check with multiple factors", {
  out <- test_data %>% 
    dplyr::select(inferior:controlled) %>% 
    psych::fa(nfactors = 2, rotate = "oblimin", fm = "pa") %>% 
    get_loadings()

  # check the loadings
  exp_pa2 <- c(0.597, 0.41, 0.409, NA, NA, NA)
  attr(exp_pa2, "label") <- "Factor 2"
  testthat::expect_equal(out$PA2, exp_pa2)

  exp_pa1 <- c(NA, NA, NA, 0.715, 0.404, NA)
  attr(exp_pa1, "label") <- "Factor 1"
  testthat::expect_equal(out$PA1, exp_pa1)

  exp_var <- c("inferior", "special", "controlled", "dominate", "deserving", "harder")
  attr(exp_var, "label") <- "Variable Name"
  testthat::expect_equal(out$variables, exp_var)

})

testthat::test_that("check when adding labels", {
  
  data <- test_data %>% 
    dplyr::select(inferior:controlled)

  # create vector of labels
  exp_labels <- c(
    "Some groups of people are simply inferior to other groups", 
    "No one group should dominate in society", 
    "Groups at the bottom are just as deserving as groups at the top",
    "It is unfair for some groups in society to receive special treatment from the government",
    "I have a harder time succeeding than my parents did",
    "Much of our lives are being controlled by plots hatched in secrecy"
  )

  ### check with dataframe
  out <- data %>% 
    psych::fa(nfactors = 1, rotate = "oblimin", fm = "pa") %>% 
    get_loadings(labels = data)
  # check labels
  attr(exp_labels, "label") <- "Variable Label"
  testthat::expect_equal(out$labels, exp_labels)
  # check the names of the columns in the output
  exp_var_names <- c("variables", "labels", "PA1", "communality", "uniqueness")
  testthat::expect_equal(names(out), exp_var_names)

  # check with named vector
  out <- data %>% 
    psych::fa(nfactors = 1, rotate = "oblimin", fm = "pa") %>% 
    get_loadings(labels = attr_var_label(data))
  testthat::expect_equal(out$labels, exp_labels)
  # check the names of the columns in the output
  exp_var_names <- c("variables", "labels", "PA1", "communality", "uniqueness")
  testthat::expect_equal(names(out), exp_var_names)

  # check with unnamed vector
  out <- data %>% 
    psych::fa(nfactors = 1, rotate = "oblimin", fm = "pa") %>% 
    get_loadings(labels = c(
      "Some groups of people are simply inferior to other groups", 
      "No one group should dominate in society", 
      "Groups at the bottom are just as deserving as groups at the top",
      "It is unfair for some groups in society to receive special treatment from the government",
      "I have a harder time succeeding than my parents did",
      "Much of our lives are being controlled by plots hatched in secrecy"
    ))
  testthat::expect_equal(out$labels, exp_labels)
  # check the names of the columns in the output
  exp_var_names <- c("variables", "labels", "PA1", "communality", "uniqueness")
  testthat::expect_equal(names(out), exp_var_names)

})

# check messages
testthat::test_that("check for message when cols is supplied", {
  testthat::expect_message(test_data %>% 
    dplyr::select(inferior:controlled) %>% 
    psych::fa(nfactors = 1, rotate = "oblimin", fm = "pa") %>% 
    get_loadings(cols = c(stick_together:business_power), sort = FALSE, threshold = 0))
})

testthat::test_that("check for message when group is supplied", {
  testthat::expect_message(test_data %>% 
    dplyr::select(inferior:controlled) %>% 
    psych::fa(nfactors = 1, rotate = "oblimin", fm = "pa") %>% 
    get_loadings(group = edu_f2, sort = FALSE, threshold = 0))
})

# Check get_loadings.factanal -------------------------------------------------------------------
testthat::test_that("Check output with sort = TRUE", {
  out <- test_data %>% 
    dplyr::select(stick_together:wall_street) %>% 
    factanal(factors = 1) %>% 
    get_loadings(sort = TRUE, threshold = 0)

  # check the names of the columns in the output
  exp_var_names <- c("variables", "Factor1", "communality", "uniqueness")
  testthat::expect_equal(names(out), exp_var_names)

  # check the variables column
  exp_var <- c("business_power", "wall_street", "head", "loyal", "stick_together", "values")
  attr(exp_var, "label") <- "Variable Name"
  testthat::expect_equal(out$variables, exp_var)

  # check the loadings
  exp_load <- c(0.849, 0.794, 0.767, 0.616, 0.572, -0.320)
  attr(exp_load, "label") <- "Factor 1"
  testthat::expect_equal(out$Factor1, exp_load)

  # check the loadings
  exp_comm <- c(0.720, 0.630, 0.589, 0.379, 0.328, 0.103)
  attr(exp_comm, "label") <- "Communality"
  testthat::expect_equal(out$communality, exp_comm)

  # check the loadings
  exp_uni <- c(0.280, 0.370, 0.411, 0.621, 0.672, 0.897)
  attr(exp_uni, "label") <- "Uniqueness"
  testthat::expect_equal(out$uniqueness, exp_uni)

})


testthat::test_that("check when adding labels", {
  
  data <- test_data %>% 
    dplyr::select(inferior:controlled)

  # create vector of labels
  exp_labels <- c(
    "Some groups of people are simply inferior to other groups", 
    "No one group should dominate in society", 
    "Groups at the bottom are just as deserving as groups at the top",
    "It is unfair for some groups in society to receive special treatment from the government",
    "I have a harder time succeeding than my parents did",
    "Much of our lives are being controlled by plots hatched in secrecy"
  )

  ### check with dataframe
  out <- data %>% 
    factanal(factors = 1) %>% 
    get_loadings(labels = data)
  # check labels
  attr(exp_labels, "label") <- "Variable Label"
  testthat::expect_equal(out$labels, exp_labels)
  # check the names of the columns in the output
  exp_var_names <- c("variables", "labels", "Factor1", "communality", "uniqueness")
  testthat::expect_equal(names(out), exp_var_names)

  ### check with named vector
  out <- data %>% 
    factanal(factors = 1) %>% 
    get_loadings(labels = attr_var_label(data))
  testthat::expect_equal(out$labels, exp_labels)
  # check the names of the columns in the output
  exp_var_names <- c("variables", "labels", "Factor1", "communality", "uniqueness")
  testthat::expect_equal(names(out), exp_var_names)

  ### check with unnamed vector
  out <- data %>% 
    factanal(factors = 1) %>% 
    get_loadings(labels = c(
      "Some groups of people are simply inferior to other groups", 
      "No one group should dominate in society", 
      "Groups at the bottom are just as deserving as groups at the top",
      "It is unfair for some groups in society to receive special treatment from the government",
      "I have a harder time succeeding than my parents did",
      "Much of our lives are being controlled by plots hatched in secrecy"
    ))
  testthat::expect_equal(out$labels, exp_labels)
  # check the names of the columns in the output
  exp_var_names <- c("variables", "labels", "Factor1", "communality", "uniqueness")
  testthat::expect_equal(names(out), exp_var_names)

})

# check messages
testthat::test_that("check for message when cols is supplied", {
  testthat::expect_message(test_data %>% 
    dplyr::select(inferior:controlled) %>% 
    factanal(factors = 1) %>% 
    get_loadings(cols = c(stick_together:business_power), sort = FALSE, threshold = 0))
})

testthat::test_that("check for message when group is supplied", {
  testthat::expect_message(test_data %>% 
    dplyr::select(inferior:controlled) %>% 
    factanal(factors = 1) %>% 
    get_loadings(group = edu_f2, sort = FALSE, threshold = 0))
})

# check loadings.data.frame ----------------------------------------------

testthat::test_that("Check default settings", {
  out <- test_data %>% 
    dplyr::select(inferior:controlled) %>% 
    get_loadings()


  # check the variables column
  exp_var <- c("inferior", "dominate", "deserving", "special", "harder", "controlled")
  attr(exp_var, "label") <- "Variable Name"
  testthat::expect_equal(out$variables, exp_var)

  # check the labels column
  exp_labels <- c(
    "Some groups of people are simply inferior to other groups", 
    "No one group should dominate in society", 
    "Groups at the bottom are just as deserving as groups at the top",
    "It is unfair for some groups in society to receive special treatment from the government",
    "I have a harder time succeeding than my parents did",
    "Much of our lives are being controlled by plots hatched in secrecy"
  )
  attr(exp_labels, "label") <- "Variable Label"
  testthat::expect_equal(out$labels, exp_labels)

  # check the loadings
  exp_load <- c(0.733,  NA, NA, NA, NA, NA)
  attr(exp_load, "label") <- "Factor 1"
  testthat::expect_equal(out$PA1, exp_load)

  # check the communality
  exp_comm <- c(0.537, 0.029, 0.088, 0.057, 0.030, 0.107)
  attr(exp_comm, "label") <- "Communality"
  testthat::expect_equal(out$communality, exp_comm)

  # check the uniqueness
  exp_uni <- c(0.463, 0.971, 0.912, 0.943, 0.970, 0.893)
  attr(exp_uni, "label") <- "Uniqueness"
  testthat::expect_equal(out$uniqueness, exp_uni)

})

testthat::test_that("check with sort = FALSE", {
  out <- test_data %>% 
    dplyr::select(top:controlled) %>% 
    get_loadings(sort = FALSE)

  # check the variables column
  exp_var <- c("top", "inferior", "dominate", "deserving", "special", "harder", "controlled")
  attr(exp_var, "label") <- "Variable Name"
  testthat::expect_equal(out$variables, exp_var)
})

testthat::test_that("check with multiple factors, varimax rotation, and minres", {
  out <- test_data %>% 
    dplyr::select(top:controlled) %>% 
    get_loadings(nfactors = 2, rotate = "varimax", fm = "minres")

  # check the names of the columns in the output
  exp_var_names <- c("variables", "labels", "MR1", "MR2", "communality", "uniqueness")
  testthat::expect_equal(names(out), exp_var_names)

  # check loading 1
  exp_mr1 <- c(0.642, 0.458, -0.438, -0.417, NA, NA, NA)
  attr(exp_mr1, "label") <- "Factor 1"
  testthat::expect_equal(out$MR1, exp_mr1)

  # check loading 2
  exp_mr2 <- c(NA, NA, 0.662, 0.433,  NA, NA, NA)
  attr(exp_mr2, "label") <- "Factor 2"
  testthat::expect_equal(out$MR2, exp_mr2)

})

testthat::test_that("check when cols is supplied", {
  # check that it works with tidy selection
  out <- test_data %>% 
    get_loadings(cols = c(top:controlled))
  exp_var <- c("inferior", "top", "dominate", "deserving", "special", "harder", "controlled")
  attr(exp_var, "label") <- "Variable Name" 
  testthat::expect_equal(out$variables, exp_var)

  # check that it works with an external vector
  exp_var <- c("inferior", "top", "dominate", "deserving", "special", "harder", "controlled")
  attr(exp_var, "label") <- "Variable Name" 
  out <- test_data %>% 
    get_loadings(cols = tidyselect::all_of(exp_var))
  testthat::expect_equal(out$variables, exp_var)

  # check when manually supplying the variable names
  out <- test_data %>% 
    get_loadings(cols = c("inferior", "top", "dominate", "deserving", "special", "harder", "controlled"))
  exp_var <- c("inferior", "top", "dominate", "deserving", "special", "harder", "controlled")
  attr(exp_var, "label") <- "Variable Name" 
  testthat::expect_equal(out$variables, exp_var)

})

testthat::test_that("check when group is supplied", {
  out <- test_data %>% 
    get_loadings(cols = c(top:controlled), group = edu_f2)

  # check the names of the columns in the output
  exp_col_names <- c("edu_f2", "variables", "labels", "PA1", "communality", "uniqueness")
  testthat::expect_equal(names(out), exp_col_names)

})

testthat::test_that("check when variables don't have labels and set na.rm to TRUE", {
  out <- test_data %>% 
    dplyr::mutate(dplyr::across(c(stick_together:business_power), as.numeric)) %>% 
    get_loadings(cols = c(stick_together:business_power), group = edu_f2, na.rm = TRUE)

  # check the names of the columns in the output
  exp_col_names <- c("edu_f2", "variables", "PA1", "communality", "uniqueness")
  testthat::expect_equal(names(out), exp_col_names)

})









test_data %>% dplyr::glimpse()
