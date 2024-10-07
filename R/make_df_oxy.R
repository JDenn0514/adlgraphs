#' Automate dataset documentation
#'
#' Creates a new R script that comes pre-filled with roxygen2
#' comments. The new file is named "df_documentation" and is located in the
#' "R/" folder in your package.
#'
#' This function is based off of \link[sinew]{makeOxygen} but is far more
#' limited. Unlike \link[sinew]{makeOxygen} this only works on objects that are
#' data.frames or tibbles. This is because the purpose of this function is to
#' automate the process of documenting the variables in a data frame by
#' leveraging the underlying variable labels.
#'
#' Another important difference between this function and \link[sinew]{makeOxygen}
#' is that this function allows you to either print the results to the home
#' console or as a new script.
#'
#' It should be noted that if a variable does not have a variable label, then
#' it will not show up in the new R script.
#'
#' @param df Name of data.frame or tibble object
#' @param title The text you want in the `@title` part of the documentation
#' @param description The text you want in the `@description` part of the
#'   dataset documentation
#' @param print Logical. Should the output print to console. Default is FALSE
#'   which writes a new R script beginning with the value of `df`
#'
#' @export
#'
#' @examples
#' # Add variable labels to iris dataset
#' library(labelled)
#' library(dplyr)
#'
#' iris_labelled <- iris %>%
#'   labelled::set_variable_labels(
#'     Sepal.Length = "Length of the flower sepal, measured in millimeters",
#'     Sepal.Width =  "Width of the flower sepal, measured in millimeters",
#'     Petal.Length = "Length of the flower petal, measured in millimeters",
#'     Petal.Width =  "Width of the flower petal, measured in millimeters",
#'     Species = "The species of flower"
#'   )
#'
#' # if you want to print to the console instead of creating a new script
#' # just add print = TRUE to the function
#' make_df_oxy(iris_labelled, print = TRUE)




# write a function that
make_df_oxy <- function(df, title, description, print = FALSE) {

  if (!missing(title)) {
    dataset_title <- title
  } else {
    dataset_title <- "DATASET_TITLE"
  }

  if (!missing(description)) {
    dataset_description <- description
  } else {
    dataset_description <- "DATASET_DESCRIPTION"
  }

  lbl <- deparse(substitute(df))
  lbl <- gsub('"', "", lbl)

  # get a list of all of the variables
  variable_labels <- attr_var_label(df)


  # Write individual item description templates
  items <- paste0(sprintf("#'   \\item{%s}{%s COLUMN_DESCRIPTION}", names(variable_labels), variable_labels), collapse = "\n")

  # Write individual item description template with var name and var label.
  # Use collapse = "\n" to concatenate the values into a single string
  # and separate them by "\n"
  new_items <- paste0("#'   \\item{", names(variable_labels), "}{", variable_labels, "}", collapse = "\n")

  # now put it all together
  ret <- paste(
    glue::glue(
      "#' @title {dataset_title},
      #' @description {dataset_description},

      #' @format A data frame with {nrow(df)} rows and {ncol(df)} variables:
      #' \\describe{
      <<new_items>>
      #'}

      <<glue::double_quote(lbl)>>", .open = "<<", .close = ">>"
    )
  )

  if (print == TRUE) {
    writeLines(ret)
  }
  else {
    # write where the file is going to live
    file <- glue::glue("R/{lbl}_documentation.R")

    # export the ret object to a new file
    writeLines(ret, file)
  }

}


