
# make the
accept_string_or_sym <- function(x) {

  # use enexpr() to capture the expressions supplied in "group"
  # enexpr returns a naked expression of the argument supplied in "group"
  # this is what allows the input to be either a string or a symbol
  x <- rlang::enexpr(x)

  if (!is.character(x)) {
    # capture group and convert to a symbol object with ensym()
    #then use as_name() to make it a string
    variable <- rlang::as_name(rlang::ensym(x))
  }


}


# determine the unique values in a variable. If the variable has value labels
# convert to a factor and get those values. If the variable is a factor or
# character string then get those. if the variable is numeric then just get the
# individual numbers
get_unique_labels <- function(x) {

  if (haven::is.labelled(x)) {
    # if group is haven_labelled

    # convert to a factor
    group_cols <- haven::as_factor(x)
    # get the unique values
    group_cols <- unique(group_cols)


  } else if (is.numeric(x) && !is.null(sjlabelled::get_labels(x))) {
    # if group is class numeric AND DOES contain value labels

    # convert to a factor
    group_cols <- sjlabelled::as_label(x)
    # get the unique values
    group_cols <- unique(group_cols)

  } else if (is.character(x) || is.factor(x)) {
    # if group is of class character or factor return x

    # get the unique values
    group_cols <- unique(x)

  } else {
    # if group is anything else (ie numeric)

    # force to a factor
    group_cols <- as.factor(x)
    # get unique values
    group_cols <- unique(group_cols)

  }

  return(group_cols)

}


# get the variable label if there is one
get_var_label <- function(x, lab) {

  # get the label for the group variable
  if (!is.null(labelled::var_label(x))) {
    # if group has a variable label

    # set group_variable_label to the variable label
    var_lab <- labelled::var_label(x)

  } else {
    # if there is no variable label then just set it to the variable name

    # set x_variable_label as the variable name
    var_lab <- lab

  }

  return(var_lab)

}

# wrap labels
label_wrap <- function(width) {
  force(width)
  function(x) {
    unlist(lapply(strwrap(x, width = width, simplify = FALSE), paste0, collapse = "\n"))
  }
}

# import the gt_add_divider function from gtExtras
gt_add_divider <- function(gt_object, columns, sides = "right", color = "grey",
                           style = "solid", weight = gt::px(2), include_labels = TRUE) {
  stopifnot("Table must be of class 'gt_tbl'" = "gt_tbl" %in% class(gt_object))

  if (isTRUE(include_labels)) {
    gt_object %>%
      gt::tab_style(
        style = gt::cell_borders(
          sides = sides,
          style = style,
          color = color,
          weight = weight
        ),
        locations = list(
          gt::cells_body(columns = {{ columns }}),
          gt::cells_column_labels(columns = {{ columns }})
        )
      )
  } else {
    gt_object %>%
      gt::tab_style(
        style = gt::cell_borders(
          sides = sides,
          style = style,
          color = color,
          weight = weight
        ),
        locations = gt::cells_body(columns = {{ columns }})
      )
  }
}

# Create a vector containing character strings comprised of all the variable
# labels for each column in a data.frame or tibble.
# write a function that will get the variable label for each column in the df
get_all_var_labels <- function(df) {
  # get a list of columns
  cols <- names(df)

  # write up a function that makes the string in the format we want
  string_fun <- function(x) {
    string <- labelled::var_label(df[[x]])
  }

  # map string_fun over each of the columns laid out earlier
  purrr::map(cols, string_fun) %>%
    setNames(cols) %>%
    # undo the list and convert to a vector
    unlist()
}


