#' Reverse a numeric function
#'
#' Reverse a numeric variable while maintaining variable and value labels
#' if available. Also adds an attribute describing the transformation the
#' original variable underwent.
#'
#' @export
#'
#' @param x A vector of class `haven_labelled` or `numeric`
#'
#' @examples
#' # create fake data
#' df <- tibble::tibble(
#'   w = sample(1:4, 10, replace = TRUE),
#'   x = sample(1:4, 10, replace = TRUE),
#'   y = sample(1:4, 10, replace = TRUE),
#'   z = sample(1:4, 10, replace = TRUE)
#' ) %>%
#'   # add value labels to x and y but not z to see what happens with no labels
#'   labelled::set_value_labels(
#'     x = c(`Strongly agree` = 1,
#'           `Somewhat agree` = 2,
#'           `Somewhat disagree` = 3,
#'           `Strongly disagree` = 4),
#'     y = c(`Strongly agree` = 1,
#'           `Somewhat agree` = 2,
#'           `Somewhat disagree` = 3,
#'           `Strongly disagree` = 4),
#'   ) %>%
#'   # set variable labels to x and z
#'   labelled::set_variable_labels(
#'     x = "This is the variable label for x",
#'     z = "This is the variable label for z"
#'   )
#'
#'
#' rev_df <- df %>% mutate(rev_w = num_rev(w))
#'
#' head(rev_df)
#'
#' str(rev_df)
#'

num_rev <- function(x) {

  # get the object's name
  x_lab <- deparse(substitute(x))

  if (haven::is.labelled(x) || (is.numeric(x) && !is.null(sjlabelled::get_labels(x)))) {

    ## reverse the values of x in a new vector "rev_x" ---------
    # get the highest value in name_vec and add 1 to it
    max_x <- max(x) + 1
    # reverse the value of x by subtracting it from name_vec_max
    # now what was a 1 is equal to a 4 and what was a 2 is equal to 3, etc.
    rev_x <- max_x - x


    ## create a new reversed named vector  "rev_name_vec" ---------
    # get the named vector using base functions
    name_vec <- attributes(x)$labels
    # get the value labels from the named vector and reverse the order of the vector
    labels <- stats::setNames(names(name_vec), name_vec) %>% rev()
    # create a new named vector with the flipped names so that when we reverse
    # the order of the values, the values will have the appropriate labels
    rev_name_vec <- stats::setNames(name_vec, labels)


    ## Create the new labelled vector using haven::labelled() -------------
    # create the labelled vector by adding the reversed labels (rev_name_vec)
    # to the reversed vector ()

    if (!is.null(labelled::var_label(x))) {

      haven::labelled(
        x = rev_x,
        labels = rev_name_vec,
        label = labelled::var_label(x)
      ) %>%
        structure(transformation = glue::glue("Reversing '{x_lab}' while maintaining correct value labels"))

    } else {
      haven::labelled(
        x = rev_x,
        labels = rev_name_vec
      ) %>%
        structure(transformation = glue::glue("Reversing '{x_lab}' while maintaining correct value labels"))
    }

  } else if (is.numeric(x) && is.null(sjlabelled::get_labels(x))) {

    max_x <- max(x) + 1
    rev_x <- max_x - x

    if (!is.null(labelled::var_label(x))) {

      haven::labelled(
        x = rev_x,
        label = labelled::var_label(x)
      ) %>%
        structure(transformation = glue::glue("Reversing '{x_lab}'"))

    } else {

      rev_x %>%
        structure(transformation = glue::glue("Reversing '{x_lab}'"))

    }

  }

}








