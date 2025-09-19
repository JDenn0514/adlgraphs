#' Flip the valence of a vector
#'
#' Reverse the valence of a vector by keeping its values consistent, but
#' reversing its value labels and providing a new variable label. By reversing
#' the valence, I mean changing a negative statement to a positive one and
#' vice versa. Please check the vignette to have a better understanding of
#' exactly what this function does (still needs to be updated).
#'
#'
#' @param x A vector of class `haven_labelled` or `numeric`
#' @param label A new variable label given to `x`.
#'
#' @returns A numeric vector of the same length as `x`
#'
#' @examples
#'
#' # create a vector
#' blue <- sample(c(1:4), size = 20, replace = TRUE)
#' # add value labels
#' attr(blue, "labels") <- c(
#'   "Strongly agree" = 1,
#'   "Somewhat agree" = 2,
#'   "Somewhat disagree" = 3,
#'   "Strongly disagree" = 4
#' )
#' # add a variable label
#' attr(blue, "label") <- "I like the color blue"
#'
#' # make it a haven-labelled object (this is just for printing purposes)
#' class(blue) <- c("haven_labelled", "vctrs_vctr", "double")
#'
#' # flip the valence
#' blue_flip <- flip_val(blue, "I dislike the color blue")
#' # show the output comapring the two
#' str(list(blue = blue, blue_flip = blue_flip))
#'
#' # can also be used inside of dplyr::mutate()
#' library(dplyr)
#'
#' new_df <- test_data %>%
#'   dplyr::mutate(
#'     # flip the valence for deserving
#'     deserving_flip = flip_val(
#'       deserving,
#'       "Groups at the bottom are not just as deserving as groups at the top"
#'     )
#'   ) %>%
#'   # keep only the relevant columns
#'   dplyr::select(deserving, deserving_flip)
#'
#' # show the output
#' str(new_df)
#'
#'
#'
#' @export
flip_val <- function(x, label = NULL) {
  # get the object's name
  x_name <- rlang::enexpr(x)

  if (missing(label)) {
    cli::cli_abort(c(
      "The argument for {.var label} is missing",
      "x" = "You must supply a new variable label in {.var label}"
    ))
  }

  if (!is.numeric(x)) {
    # if the vector is not numeric at all
    cli::cli_abort(c(
      "The argument for {.var x} must be of class {.cls numeric} ",
      "x" = "You've supplied {x_name} which is an object of class {.cls {class(x)}}"
    ))
  }

  if (!is.null(attr_val_labels(x))) {
    # get the named vector using base functions
    name_vec <- attr_val_labels(x)
    # get the value labels from the named vector and reverse the order of the vector
    labels <- stats::setNames(names(name_vec), name_vec) %>% rev()
    # create a new named vector with the flipped names so that when we reverse
    # the order of the values, the values will have the appropriate labels
    rev_name_vec <- stats::setNames(name_vec, labels)

    # set the value labels
    attr(x, "labels") <- rev_name_vec
    # set the transformation attribute
    attr(x, "transformation") <- paste0(
      "Flipped the valance of `",
      x_name,
      "` so it is now negative by updating the label and value labels accordingly."
    )
  } else {
    # if there aren't any labels then just set the transformation attribute
    attr(x, "transformation") <- paste0(
      "Flipped the valance of `",
      x_name,
      "` so it is now negative by updating the label accordingly."
    )
  }

  if (!is.character(label)) {
    # supply an error if the label is not a string or character
    cli::cli_abort(c(
      "{.var label} must be of class {.cls character} ",
      "x" = "You've supplied an object of class {.cls {class(label)}}"
    ))
  }

  # set the variable label using the provided string
  attr(x, "label") <- label
  # set the classes
  class(x) <- c("haven_labelled", "vctrs_vctr", typeof(x))

  x
}
