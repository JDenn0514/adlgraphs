

# make_question_index <- function(data, cols) {

#   data_name <- attr({{ data }}, "data_name")

#   lbl <- deparse(substitute(data))

#   data <- data %>% dplyr::select({{ cols }})

#   col_names <- colnames(data)

#   out <- purrr::map(col_names, function(x) get_question_and_response(x, {{ data }})) %>% 
#     unlist() %>% 
#     stringr::str_c(collapse = "")

#   output <- paste(
#     glue::glue(
#       '---
#       title: "My document"
#       format: docx
#       output-file: "{data_name}.docx"
#       ---

#       {out}'
#     )
#   )


#   # write where the file is going to live
#   file <- glue::glue("R/{lbl}_documentation.qmd")

#   # export the ret object to a new file
#   writeLines(output, file)
# }


# get_question_and_response <- function(x, data) {
  
#   var_lab <- attr_var_label(x, data)

#   if (is.null(var_lab)) {
#     x_name <- deparse(substitute(x))
#     stop(glue::glue("No variable label in `{x_name}`"))
#   }

#   var_lab <- paste("* **Question:", var_lab, "**\n")

#   value_labels <- attr_val_labels(x, data) %>% setNames(names(.), .) %>% as.list()
#   value_labels_full <- purrr::map(value_labels, function(x) paste("\t \t -", x, "\n")) %>% unlist()

#   question_preface <- attr_question_preface(x, data)
#   question_preface_full <- paste("\t + Question Preface:", question_preface, "\n")

#   data_name <- attr({{ data }}, "data_name")
#   data_name <- paste("\t \t -", data_name, "\n")

#   if (is.null(value_labels)) {
#     if (is.null(question_preface)) {

#       output <- c(
#         paste(var_lab),
#         "\t + Survey: \n",
#         paste(data_name),
#         "\n"
#       )

#     } else {

#       output <- c(
#         paste(var_lab),
#         paste(question_preface_full),
#         "\t + Survey: \n",
#         paste(data_name),
#         "\n"
#       )

#     }
#   } else {
#     if (is.null(question_preface)) {

#       output <- c(
#         paste(var_lab),
#         "\t + Response Options: \n", 
#         paste(value_labels_full),
#         "\t + Survey: \n",
#         paste(data_name),
#         "\n"
#       )

#     } else {

#       output <- c(
#         paste(var_lab),
#         "\t + Response Options: \n", 
#         paste(value_labels_full),
#         paste(question_preface_full),
#         "\t + Survey: \n",
#         paste(data_name),
#         "\n"
#       )

#     }

#   }

#   return(output)

# }



