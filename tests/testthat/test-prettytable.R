

# # Helper function to extract data from a gt table for comparison
# extract_gt_data <- function(gt_table) {
#   # Extract the main data frame.  This is the *displayed* data.
#   main_data <- gt_table[["_data"]]

#   # Extract column names
#   column_names <- names(main_data)

#   # Extract spanner information (for grouped columns)
#   spanners <- gt_table[["_spanners"]]

#   # Extract the table header
#   header <- gt_table[["_heading"]]

#   # Extract the footnotes
#   footnotes <- gt_table[["_footnotes"]]

#   # Return relevant information for testing
#   list(
#     data = main_data,
#     column_names = column_names,
#     spanners = spanners,
#     header = header,
#     footnotes = footnotes
#   )
# }


# # Test case 1: Basic table structure without groups
# testthat::test_that("Table structure is correct without groups", {
#   # Create a sample dataset with groups and labels
#   data_test <- funky_freqs(test_data, top, wt = wts)
  
#   result_gt <- prettytable(data_test)
#   result_data <- extract_gt_data(result_gt)
#   result_data$column_names

#   # Check column names (order matters here!)
#   expected_cols_with_groups <- c("top", "pct", "n")
#   testthat::expect_equal(result_data$column_names, expected_cols_with_groups)

#   # Check that the first column is 'variable'
#   testthat::expect_equal(names(result_data$data)[1], "top")

#     # Check the table header
#   testthat::expect_equal(result_data$header$title, 'Calculating the frequencies for "An ideal society requires some groups to be on top and others to be on the bottom"')

# })


# # Test case 2: Check content of the table (numbers, percentages)
# testthat::test_that("Table content is correct without groups", {
#   data_test <- funky_freqs(test_data, top, wt = wts)

#   result_gt <- prettytable(data_test)
#   result_data <- extract_gt_data(result_gt)$data

  
#   testthat::expect_equal(
#     result_data$n, 
#     c(25, 86, 75, 59) %>% structure(label = "N")
#   )

#   testthat::expect_equal(
#     result_data$pct, 
#     c("10.25%", "35.13%", "30.59%", "24.03%") %>% 
#       structure(label = "Percent", transformation = "Added a `%` symbol to `x$pct`")
#   )

# })


# # Test case 3: Table structure and content with groups
# testthat::test_that("Table structure is correct without groups", {
#   # Create data without groups
#   data_no_groups <- data_test %>% select(variable, weights)
#   attr(data_no_groups, "variable_name") <- quo(variable)
#   attr(data_no_groups, "variable_label") <- quo("Variable Label")
#   attr(data_no_groups, "dataset") <- data_no_groups # Add the dataset attribute

#   result_gt <- prettytable(data_no_groups)
#   result_data <- extract_gt_data(result_gt)

#   # Check column names
#   expected_cols_no_groups <- c("variable", "Percent", "N")
#   testthat::expect_equal(result_data$column_names, expected_cols_no_groups, info = "Column names are incorrect without groups")

#   # Check that the first column is 'variable'
#   testthat::expect_equal(names(result_data$data)[1], "variable", info = "First column should be 'variable'")

#   # Check for the table header
#   expect_equal(result_data$header$title, 'Calculating the frequencies for "Variable Label"', info = "Table header is incorrect")

# })

# # Test case 4:  Testing the show_genpop option
# testthat::test_that("Table includes general population data when show_genpop is TRUE", {
  
#   result_gt <- prettytable(data_test, show_genpop = TRUE)
#   result_data <- extract_gt_data(result_gt)
#   # Check for the existence of the general population columns
#   expect_true(
#     " _General Population_pct" %in% result_data$column_names, 
#     info = "General population pct column is missing"
#   )
#   expect_true(
#     " _General Population_n" %in% result_data$column_names, 
#     info = "General population n column is missing"
#   )

#   # Check column order when show_genpop = TRUE.  This is more complex because of the added column.
#   expected_cols_with_genpop <- c("variable", "Group One Label_A_Percent", "Group One Label_A_N", "Group One Label_B_Percent", "Group One Label_B_N",
#                                  "Group Two Label_X_Percent", "Group Two Label_X_N", "Group Two Label_Y_Percent", "Group Two Label_Y_N", "Group Two Label_Z_Percent", "Group Two Label_Z_N",
#                                  " ", " _General Population_pct", " _General Population_n")
#   # column names 
#   testthat::expect_equal(
#     result_data$column_names, 
#     expected_cols_with_genpop, 
#     info = "Column names are incorrect with show_genpop = TRUE"
#   )

# })

# # Test case 5: Testing footnotes
# testthat::test_that("Footnotes are added correctly", {
#   result_gt <- prettytable(data_test)
#   result_data <- extract_gt_data(result_gt)

#   # Check that the correct number of footnotes is present
#   expected_footnote_count <- 2  # Based on your example data and function
#   expect_length(result_data$footnotes$footnotes, expected_footnote_count, info = "Incorrect number of footnotes")

#   # Check the content of the footnotes.  Order is important here.
#   expected_footnote_texts <- c("Group One Label", "Group Two Label")
#   actual_footnote_texts <- sapply(result_data$footnotes$footnotes, function(x) x$footnote) # Extract the footnote text
#   testthat::expect_equal(actual_footnote_texts, expected_footnote_texts, info = "Incorrect footnote texts")

# })



