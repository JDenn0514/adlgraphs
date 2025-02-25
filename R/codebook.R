#' Create a data codebook
#'
#' This function takes a `data.frame` and creates a codebook. The new
#' object is a `data.frame` where each row contains different metadata info
#' found within each column from the original data set. Each column in the new
#' data set represents a different element of the underlying metadata. This is
#' similar to `labelled::look_for()` but shows more of the underlying metadata.
#'
#' Currently, the new data set provided by this function has the following columns:
#'
#'  * pos - The position of the variable.
#'
#'  * variable - The name of the variable.
#'
#'  * label - The variable label.
#'
#'  * levels - If the variable is a factor, the factor levels are listed here.
#'
#'  * value_labels - If the variable is a labelled vector, the value labels are
#'    listed here.
#'
#'  * transformation - An explanation of any potential data transformations the
#'    variable underwent is listed here. This useful if you want to remember how
#'    a variable was created without going back to the cleaning script.
#'
#'  * question_preface - This contains the question preface. To elaborate, some
#'    questions in surveys enable respondents to select multiple responses. Each
#'    response gets it's own variable in the data. The value listed here is
#'    supposed to contain the text that prefaced the response options. The
#'    actual response option is listed under 'label'.
#'
#'  * survey flow - This is used to indicate if there was an experiment or some
#'    sort of branching involved in the survey flow.
#'
#'  * note - A miscellaneous attribute in which you can add random information
#'    about the variable that doesn't fit in the other attributes.
#'
#'  * class - The class attribute of the variable.
#'
#'  * type - The type of the variable.
#'
#'  * missing - Indicates how many missing values there are.
#'
#'  * range - If a numeric variable, shows the range of the values.
#'
#' @param data An object of class `data.frame` or `tibble`
#' 
#' @returns A tibble with with the number of rows as columns in `data`
#'
#' @examples
#' # create the codebook
#' test_data_codebook <- codebook(test_data)
#' # view the codebook
#' test_data_codebook
#'
#'
#' @export


codebook <- function(data) {

  data_lab <- deparse(substitute(data))

  if (!is.data.frame(data)) {
    cli::cli_abort(c(
      "{.var data} must be of class {.cls data.frame}",
      "x" = "You've supplied an object of class {.cls {class(data)}}"
    ))
  }

  # attr the variable names
  names <- names(data)
  # get the number of variables
  len <- length(names)

  if (!len) stop("there are no names to search in that object")

  # get the variable labels
  labels <- attr_var_label(data)

  # get the factor levels
  levels <- attr_levels({{ data }})
  # get the value labels
  value_labels <- flatten_labelled_vec({{ data }})

  # get the transformation attribute
  transformation <- attr_transformation({{ data }})

  # get the note attribute
  note <- attr_note({{ data }})

  # get the question_preface attribute
  question_preface <- attr_question_preface({{ data }})

  # get the survey_flow attribute
  survey_flow <- attr_survey_flow({{ data }})

  pos <- which(names %in% names)

  # reordering according to pos
  # not forgetting that some variables don't have a label
  if (length(labels)) {
    res <- dplyr::tibble(pos = pos, variable = names[pos], label = labels[names[pos]])
  } else {
    res <- dplyr::tibble(pos = pos, variable = names[pos], label = NA_character_)
  }

  unique_values <- function(x) {
    length(unique(x))
  }
  generic_range <- function(x) {
    if (all(unlist(lapply(x, is.null)))) {
      return(NULL)
    }
    if (all(is.na(x))) {
      return(NULL)
    }

    r <- suppressWarnings(try(range(x, na.rm = TRUE), silent = TRUE))
    if (inherits(r, "try-error")) {
      return(NULL)
    }

    r
  }

  res <- res %>%
    dplyr::mutate(
      levels = levels,
      value_labels = value_labels,
      transformation = transformation,
      question_preface = question_preface,
      survey_flow = survey_flow,
      note = note,
      col_type = unlist(lapply(data, vctrs::vec_ptype_abbr)),
      class = lapply(data, class),
      type = unlist(lapply(data, typeof)),
      missing = unlist(lapply(data, n_missing)), # retrocompatibility
      unique_values = unlist(lapply(data, unique_values)),
      range = lapply(data, generic_range)
    ) %>% 
    tidyr::unnest(value_labels, keep_empty = TRUE) %>% 
    tidyr::unnest(question_preface, keep_empty = TRUE)

  return(res)

}

