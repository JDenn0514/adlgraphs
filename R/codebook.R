#' Create a df codebook
#'
#' This function takes a `df.frame` and creates a codebook. The new
#' object is a `df.frame` where each row contains different metadf info
#' found within each column from the original df set. Each column in the new
#' df set represents a different element of the underlying metadf. This is
#' similar to `labelled::look_for()` but shows more of the underlying metadf.
#'
#' Currently, the new df set provided by this function has the following columns:
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
#'  * transformation - An explanation of any potential df transformations the
#'    variable underwent is listed here. This useful if you want to remember how
#'    a variable was created without going back to the cleaning script.
#'
#'  * question_preface - This contains the question preface. To elaborate, some
#'    questions in surveys enable respondents to select multiple responses. Each
#'    response gets it's own variable in the df. The value listed here is
#'    supposed to contain that text that prefaced the response options. The
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
#'  * missing - Indicaets how many missing values there are.
#'
#'  * range - If a numeric variable, shows the range of the values.
#'
#' @param df An object of type df.frame or tibble
#'
#' @examples
#' # create the codebook
#' test_data_codebook <- codebook(test_data)
#' # view the codebook
#' test_data_codebook
#'
#'
#' @export


codebook <- function(df) {

  df_lab <- deparse(substitute(df))

  if (!is.data.frame(df) || !tibble::is_tibble(df)) {
    cli::cli_abort(c(
      "{.var df} must be of class {.cls tbl_df}, {.cls tbl}, or {.cls df.frame}",
      "x" = "You've supplied an object of class {.cls {class(df)}}"
    ))
  }

  # attr the variable names
  names <- names(df)
  # get the number of variables
  len <- length(names)

  if (!len) stop("there are no names to search in that object")

  # get the variable labels
  labels <- get_all_var_labels(df)

  # get the factor levels
  factor_levels <- get_all_factor_levels({{ df }})

  levels <- get_all_factor_levels({{ df }})
  # get the value labels
  value_labels <- get_all_val_labels({{ df }})

  # get the transformation attribute
  transformation <- get_all_transformation({{ df }})

  # get the note attribute
  note <- get_all_note({{ df }})

  # get the question_preface attribute
  question_preface <- get_all_question_preface({{ df }})

  # get the survey_flow attribute
  survey_flow <- get_all_survey_flow({{ df }})

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
      col_type = unlist(lapply(df, vctrs::vec_ptype_abbr)),
      class = lapply(df, class),
      type = unlist(lapply(df, typeof)),
      missing = unlist(lapply(df, n_missing)), # retrocompatibility
      unique_values = unlist(lapply(df, unique_values)),
      range = lapply(df, generic_range)
    )

  return(res)

}

