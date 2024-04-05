#
#
#
# get_fa_table <- function(
#     model,
#     labels = NULL,
#     threshold = 0.4
#   ) {
#
#   if (!is.null(labels)) {
#     labels <- get_all_var_labels(labels)
#   }
#
#   # get the number of factors ()
#   n <- model$factors
#
#
#
#   factor_analysis <- model %>%
#     # get a pretty version of the factor loadings
#     model_parameters(
#       # sort the variables
#       sort = TRUE,
#       # set the loading cut off at 0.4
#       threshold = threshold,
#       # add variable labels
#       labels = labels
#     ) %>%
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
#
# }
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
