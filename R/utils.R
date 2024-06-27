
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

#' Identify correct font path based on filename
#'
#' Taking a list of font paths, search for a specific filename. If a perfect
#' match is found, return that path.
#'
#' @param filename the complete file name, less a .otf or .ttf extension.
#' @param path a vector of filepaths
#'
#' @noRd
find_path <- function(filename, paths){
  result <- grep(paste0("(\\\\|/)", filename, ".[ot]tf$"), paths, value = TRUE)

  if(length(result) >= 1){
    return(result[1])
  } else {
    stop(
      paste0("Font '", filename, "' not found."),
      call. = FALSE)
  }
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

  if (is.numeric(x) && attr_val_labels(x)) {
    # if group is haven_labelled

    # convert to a factor
    group_cols <- make_factor(x)
    # get the unique values
    group_cols <- forcats::fct_unique(group_cols)


  }  else if (is.character(x) || is.factor(x)) {
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
  if (!is.null(attr_var_label(x))) {
    # if group has a variable label

    # set group_variable_label to the variable label
    var_lab <- attr_var_label(x)

  } else {
    # if there is no variable label then just set it to the variable name

    # set x_variable_label as the variable name
    var_lab <- lab

  }

  return(var_lab)

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


# create quantiles
split_quantile <- function(x = NULL,
                           type = NULL) {
  if(length(x) < 2) {
    stop("The `x` argument provided to quantile split must be non-null and ",
         "length at least 2.")
  }
  if(!is.numeric(type)) {
    stop("The `type` argument provided to quantile split must be non-null and ",
         "numeric.")
  }

  cut(x, breaks = stats::quantile(x, probs = seq(0, 1, length.out = type + 1)),
      labels = 1:type,
      include.lowest = TRUE)
}


# Check if all data points are inside bounds. If not, warn and remove them.
fit_data_to_bounds <- function(bounds, x, w) {
  is_inside_bounds <- (bounds[1] <= x) & (x <= bounds[2])
  w_sum <- 1
  if (!all(is_inside_bounds)) {
    cli::cli_warn("Some data points are outside of `bounds`. Removing them.")
    x <- x[is_inside_bounds]
    w <- w[is_inside_bounds]
    w_sum <- sum(w)
    if (w_sum > 0) {
      w <- w / w_sum
    }
  }

  return(list(x = x, w = w, w_sum = w_sum))
}

# Update density estimation to mitigate boundary effect at known `bounds`:
# - All x values will lie inside `bounds`.
# - All y-values will be updated to have total probability of `bounds` be
#   closer to 1. This is done by reflecting tails outside of `bounds` around
#   their closest edge. This leads to those tails lie inside of `bounds`
#   (completely, if they are not wider than `bounds` itself, which is a common
#   situation) and correct boundary effect of default density estimation.
#
# `dens` - output of `stats::density`.
# `bounds` - two-element vector with left and right known (user supplied)
#   bounds of x values.
# `from`, `to` - numbers used as corresponding arguments of `stats::density()`
#   in case of no boundary correction.
reflect_density <- function(dens, bounds, from, to) {
  # No adjustment is needed if no finite bounds are supplied
  if (all(is.infinite(bounds))) {
    return(dens)
  }

  # Estimate linearly with zero tails (crucial to account for infinite bound)
  f_dens <- stats::approxfun(
    x = dens$x, y = dens$y, method = "linear", yleft = 0, yright = 0
  )

  # Create a uniform x-grid inside `bounds`
  left <- max(from, bounds[1])
  right <- min(to, bounds[2])
  out_x <- seq(from = left, to = right, length.out = length(dens$x))

  # Update density estimation by adding reflected tails from outside `bounds`
  left_reflection <- f_dens(bounds[1] + (bounds[1] - out_x))
  right_reflection <- f_dens(bounds[2] + (bounds[2] - out_x))
  out_y <- f_dens(out_x) + left_reflection + right_reflection

  list(x = out_x, y = out_y)
}


n_missing <- function(x) {
  sum(is.na(x) | is.null(x))
}
