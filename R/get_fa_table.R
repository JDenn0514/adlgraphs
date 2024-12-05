
get_loadings <- function(
  model, 
  labels, 
  threshold = 0.4, 
  print = "short"
) {
  UseMethod("get_loadings")
}

#' @export
get_loadings.default <- function(
  model, 
  labels, 
  threshold = 0.4, 
  print = "short"
) {

  n <- model$factors

  loadings <- tibble::as_tibble(unclass(model$loadings), rownames = "variables") %>% 
      dplyr::mutate(
        communality = model$communality,
        uniqueness = model$uniqueness
      )
    
  if (!is.null(labels)) {
    if (is.character(labels)) {

      loadings <- loadings %>% 
        dplyr::mutate(labels = labels) %>% 
        dplyr::relocate(labels, .after = "variables") 

    } else if (is.data.frame(labels)) {

      labels <- attr_var_label(labels)
      loadings <- loadings %>% 
        dplyr::mutate(labels = labels) %>% 
        dplyr::relocate(labels, .after = "variables") 
    }
      
  }
  # get the columns with loadings in them
  loading_cols <- 3:(n + 2)
  
  # 
  loadings[, loading_cols][abs(loadings[, loading_cols]) < threshold] <- NA

  loadings 

}

get_loadings.data.frame <- function(
  model, 
  labels, 
  threshold = 0.4, 
  print = "short",
  nfactors = 1,
  fm = "pa",
  rotate = "oblimin"
) {

  model <- model %>% 
    psych::fa(
      nfactors = nfactors,
      fm = fm,
      rotate = rotate
    )
  
  get_loadings(model, labels = labels, threshold = threshold)
  
  
}

get_loadings.grouped_df <- function(
  model, 
  labels, 
  threshold = 0.4, 
  print = "short",
  nfactors = 1,
  fm = "pa",
  rotate = "oblimin"
) {

  # make it a nested data set
  nest_data <- data %>% tidyr::nest()

  # get the groups 
  # we will combine this with the correlations
  just_groups <- nest_data %>% dplyr::select(-data)

  # make the correlation dataframe
  load_df <- purrr::map(
    # we are iterating over the data column
    nest_data$data, 
    # use wtd_corr to get the individuals correlations
    ~get_loadings.data.frame() 
  ) %>% 
    # bind the rows together
    dplyr::bind_rows()

}




# get_fa_table <- function(
#     model,
#     labels = NULL,
#     threshold = 0.4
#   ) {

  
  
  
  
  
#   if (!is.null(labels)) {
#     labels <- attr_var_label(labels)
#   }

#   # get the number of factors ()
#   n <- model$factors



#   factor_analysis <- model %>%
    
#     # round numeric columns to second decimal
#     mutate(across(where(is.numeric),
#                   ~round(.x, 2))) %>%
#     # pivot the table
#     pivot_longer_values(
#       cols = c(PA1:PA2),
#       names_to = "Factor",
#       values_to = "Loading"
#     ) %>%
#     # move loadings after the labels
#     relocate("Loading", .after = "Label") %>%
#     # drop NA loadings
#     drop_na(Loading) %>%
#     # arrange based on the factor
#     arrange(Factor) %>%
#     #select(-Variable) %>%
#     gt::gt(groupname_col = "Factor")

# }






















