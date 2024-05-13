
# my own functions --------------------------------------------------------

# clean the racial groups
clean_race <- function(df, x) {

  # get the label
  label <- attributes(df[[x]])$labels
  # get the value label so we
  label <- stats::setNames(names(label), label)

  if (label == "White") {
    name <- "white_b"
  } else if (label == "Black or African-American") {
    name <- "black_b"
  } else if (label == "Asian or Asian-American") {
    name <- "asian_b"
  } else if (label == "American Indian or Alaska Native") {
    name <- "native_b"
  } else if (label == "Native Hawaiian or other Pacific Islander") {
    name <- "hawaiian_b"
  } else if (label == "Some other race or origin") {
    name <- "other_b"
  }

  df %>%
    dplyr::mutate(
      !!name := dplyr::case_match(
        .data[[x]],
        1 ~ 1,
        .default = 0
      )
    )

}


# make the
accept_string_or_sym <- function(x) {

  # use enexpr() to capture the expressions supplied in "group"
  # enexpr returns a naked expression of the argument supplied in "group"
  # this is what allows the input to be either a string or a symbol
  x <- rlang::enexpr(x)

  if (!is.character(x)) {
    # capture group and convert to a symbol object with ensym()
    #then use as_name() to make it a string
    x <- rlang::as_name(rlang::ensym(x))
  }

  return(x)

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
    group_cols <- forcats::fct_unique(group_cols)


  } else if (is.numeric(x) && !is.null(sjlabelled::get_labels(x))) {
    # if group is class numeric AND DOES contain value labels

    # convert to a factor
    group_cols <- sjlabelled::as_label(x)
    # get the unique values
    group_cols <- forcats::fct_unique(group_cols)

  } else if (is.character(x) || is.factor(x)) {
    # if group is of class character or factor return x

    # get the unique values
    group_cols <- forcats::fct_unique(x)

  } else {
    # if group is anything else (ie numeric)

    # force to a factor
    group_cols <- as.factor(x)
    # get unique values
    group_cols <- forcats::fct_unique(group_cols)

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



# functions from other packages -------------------------------------------

#
check_factor <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (is.character(x)) {
    factor(x)
  } else if (is.factor(x)) {
    x
  } else {
    cli::cli_abort(
      "{.arg {arg}} must be a factor or character vector, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
}

# get unique factors (from forcats)
fct_unique <- function(f) {
  f <- check_factor(f)

  levels <- levels(f)
  out <- seq_along(levels)

  # Ensure out includes any implicit missings
  if (anyNA(f)) {
    out <- c(out, NA_integer_)
  }

  structure(
    out,
    levels = levels,
    class = c(if (is.ordered(f)) "ordered", "factor")
  )
}

# wrap labels from the scales package
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


# make single column facets from ggforce
facet_col <- function(facets, scales = "fixed", space = "fixed",
                      shrink = TRUE, labeller = "label_value",
                      drop = TRUE, strip.position = 'top') {
  space <- match.arg(space, c('free', 'fixed'))
  facet <- ggplot2::facet_wrap(facets, ncol = 1, scales = scales, shrink = shrink, labeller = labeller, drop = drop, strip.position = strip.position)
  params <- facet$params

  params$space_free <- space == 'free'
  ggproto(NULL, FacetCol, shrink = shrink, params = params)
}

# from ggforce
FacetCol <- ggplot2::ggproto('FacetCol', ggplot2::FacetWrap,
                    draw_panels = function(self, panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
                      combined <- ggproto_parent(FacetWrap, self)$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
                      if (params$space_free) {
                        heights <- vapply(layout$PANEL, function(i) diff(ranges[[i]]$y.range), numeric(1))
                        panel_heights <- unit(heights, "null")
                        combined$heights[panel_rows(combined)$t] <- panel_heights
                      }
                      combined
                    }
)






