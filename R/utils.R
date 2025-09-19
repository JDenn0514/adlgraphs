# my own functions --------------------------------------------------------

# this function updates the column names so that they are N or Percent
fix_pct <- function(x) {
  # extract the columns from the gt data
  columns <- colnames(x[["_data"]])
  # get only the columns with "n_" or "pct_"
  og_cols <- grep("_n|_pct", columns, value = TRUE)
  # extract only the part of the string with "n_" or "pct_" in it
  new_cols <- unlist(regmatches(og_cols, m = regexec("_n|_pct", og_cols)))

  # clean up the names of the columns
  # replace "n_" with "N" in the
  new_cols <- gsub("_n", "N", new_cols)
  # replace "pct_" with "Percent", use . as placeholder
  new_cols <- gsub("_pct", "Percent", new_cols)

  # set the names of new_names using the original column names
  names(new_cols) <- og_cols
  return(new_cols)
}

# this function sorts the columns in a wide data.frame
sort_cols <- function(x, group_names, variable_name, og) {
  # keep only the strings starting with "n_" or "pct_"
  new_cols <- grep("^(n_|pct_)", colnames(x), value = TRUE)

  # make a list by splitting each string in new_cols by "_"
  list_strings <- strsplit(new_cols, "_", fixed = TRUE)
  # use do.call to perform rbind over each element in list_strings
  #   and combine all resulting vectors together as a matrix
  mat <- do.call(rbind, list_strings)
  # convert the matrix into a data frame
  data <- data.frame(mat)

  # rename the columns
  names(data) <- c("pct_n", group_names)

  data[group_names] <- character_to_factor(data, og, group_names)

  # the sort data by the columns in group_names
  data <- sort_by(data, data[group_names])
  return(data)
}


character_to_factor <- function(new, old, cols) {
  # convert character vectors to factors
  lapply(
    # perform the function only over the common data frames
    cols |> stats::setNames(nm = _),
    # write the anonymous function
    \(y) {
      if (!is.null(levels(old[[y]]))) {
        # if old[[y]] has levels
        # set new[[y]] as factor with levels from old[[y]]
        factor(new[[y]], levels(old[[y]]))
      } else {
        # if old[[y]] does not have levels coerce to factor
        as.factor(new[[y]])
      }
    }
  )
}

# helper function for doing grouped data analysis
group_analysis_helper <- function(data, cols) {
  # extract the df in the "groups" attribute
  group_labs <- attr(data, "groups") %>%
    # remove the .rows column
    dplyr::select(-`.rows`)
  # get the column names, these are the grouping variables
  group_labs <- colnames(group_labs)
  leng_groups <- length(group_labs)

  if (!missing(cols)) {
    # get a data set with only the grouping variables
    data_groups <- data %>% dplyr::select(group_labs)

    # use enquo to defuse the argument
    expr <- rlang::enquo(cols)
    # resume evaluation using eval_select to get the positions of the variables
    pos <- tidyselect::eval_select(expr, data = data)
    # Use the vector of locations returned by eval_select() to subset
    # and rename the input data.
    data_cols <- rlang::set_names(data[pos], names(pos))

    # combine the two
    data <- dplyr::bind_cols(data_groups, data_cols)
  }

  # make it a nested data set
  nest_data <- data %>%
    tidyr::nest() %>%
    tidyr::drop_na(tidyselect::everything())

  if (leng_groups > 1) {
    for (n in c(1:leng_groups)) {
      if (n == 1) {
        string <- paste('.data[[group_labs[1]]]')
      } else {
        string <- paste0(string, ', " - ", .data[[group_labs[', n, ']]]')
      }
      new_string <- paste0("paste0(", string, ")")
    }

    # get the groups
    # we will combine this with the correlations
    just_groups <- nest_data %>%
      # remove the data column so it's just the groups
      dplyr::select(-data) %>%
      # combined the grouping variabels
      dplyr::mutate(groups_combined = eval(parse(text = new_string))) %>%
      # extract it as a vector
      dplyr::pull(groups_combined)
  } else {
    # get the groups
    # we will combine this with the correlations
    just_groups <- nest_data %>%
      dplyr::pull(-data)
  }

  out <- list(nest_data, just_groups, group_labs)
  names(out) <- c("nest_data", "just_groups", "group_labs")

  return(out)
}


# clean the racial groups
clean_race <- function(df, x) {
  # get the label
  label <- attr(df[[x]], "labels", exact = TRUE)
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
  } else if (label == "Asian") {
    name <- "asian_b"
  } else if (label == "Hispanic or Latino") {
    name <- "hispanic_b"
  } else if (label == "Middle Eastern or North African") {
    name <- "mena_b"
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
find_path <- function(filename, paths) {
  result <- grep(paste0("(\\\\|/)", filename, ".[ot]tf$"), paths, value = TRUE)

  if (length(result) >= 1) {
    return(result[1])
  } else {
    stop(
      paste0("Font '", filename, "' not found."),
      call. = FALSE
    )
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


#' Select variables from the group variable
#' @param group `tidy_select` columns to group the data by
#' @param data The data set that the columns are from (this to make sure they exist)
#'
#' @keywords internal
select_groups <- function(group, data) {
  group_vars <- as.character(rlang::quo_squash(rlang::enexpr(group)))
  group_vars <- group_vars[group_vars != "c"]

  if (!all(group_vars %in% colnames(data))) {
    data_name <- substitute(data)
    group_vars <- group_vars[c(!group_vars %in% colnames(data))]
    cli::cli_abort(c(
      "Column `{group_vars}` is not found in `{data_name}`",
      "i" = "Make sure all variables supplied to {.var group} are present in {.var data}"
    ))
  }
  return(group_vars)
}

# a low level function for getting variable labels that powers the attr_var_label
low_var_label <- function(x, data, if_null = NULL) {
  x_name <- rlang::quo_get_expr(rlang::expr({{ x }}))

  if (missing(data)) {
    x <- attr(x, "label", exact = TRUE)
  } else {
    x <- attr(data[[x]], "label", exact = TRUE)
  }

  if (is.null(x) && !is.null(if_null)) {
    if (if_null == "name") {
      x <- x_name
    } else if (if_null == "NA" && !is.null(if_null)) {
      x <- NA
    }
  }
  x
}

# check_groups <- function(group, data) {
#   # if the data is grouped, use dplyr::group_vars to get them, else set to NULL
#   group_names <- if(inherits(data, "grouped_df")) setdiff(names(attr(model, "groups")), ".rows") else NULL
#   # if group arg is missing set to NULL, else use as.character(substitute()) to capture it
#   group_vars <- if (missing(group)) NULL else select_groups({{ group }}, data)
#   # remove the "c" from the group_vars vector if it is there
#   group_vars <- group_vars[group_vars != "c"]
#   # combine group_names and group_vars for the final vector of group names
#   # use unique to make sure there aren't any duplicates
#   group_names <- unique(c(group_names, group_vars))

#   group_names
# }

# functions from other packages -------------------------------------------

# from dplyr
eval_select_by <- function(
  by,
  data,
  error_call = rlang::caller_env()
) {
  # this works with
  out <- tidyselect::eval_select(
    expr = by,
    data = data,
    allow_rename = FALSE,
    error_call = error_call
  )
  names(out)
}

# from
check_factor <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
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
    unlist(lapply(
      strwrap(x, width = width, simplify = FALSE),
      paste0,
      collapse = "\n"
    ))
  }
}


# import the gt_add_divider function from gtExtras
gt_add_divider <- function(
  gt_object,
  columns,
  sides = "right",
  color = "grey",
  style = "solid",
  weight = gt::px(2),
  include_labels = TRUE
) {
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
facet_col <- function(
  facets,
  scales = "fixed",
  space = "fixed",
  shrink = TRUE,
  labeller = "label_value",
  drop = TRUE,
  strip.position = 'top'
) {
  space <- match.arg(space, c('free', 'fixed'))
  facet <- ggplot2::facet_wrap(
    facets,
    ncol = 1,
    scales = scales,
    shrink = shrink,
    labeller = labeller,
    drop = drop,
    strip.position = strip.position
  )
  params <- facet$params

  params$space_free <- space == 'free'
  ggplot2::ggproto(NULL, FacetCol, shrink = shrink, params = params)
}

# from ggforce
FacetCol <- ggplot2::ggproto(
  'FacetCol',
  ggplot2::FacetWrap,
  draw_panels = function(
    self,
    panels,
    layout,
    x_scales,
    y_scales,
    ranges,
    coord,
    data,
    theme,
    params
  ) {
    combined <- ggplot2::ggproto_parent(FacetWrap, self)$draw_panels(
      panels,
      layout,
      x_scales,
      y_scales,
      ranges,
      coord,
      data,
      theme,
      params
    )
    if (params$space_free) {
      heights <- vapply(
        layout$PANEL,
        function(i) diff(ranges[[i]]$y.range),
        numeric(1)
      )
      panel_heights <- ggplot2::unit(heights, "null")
      combined$heights[ggplot2::panel_rows(combined)$t] <- panel_heights
    }
    combined
  }
)


# create quantiles, from gtools

quantcut <- function(x, q = 4, na.rm = TRUE, ...) {
  if (length(q) == 1) {
    q <- seq(0, 1, length.out = q + 1)
  }

  quant <- stats::quantile(x, q, na.rm = na.rm)
  dups <- duplicated(quant)

  if (any(dups)) {
    flag <- x %in% unique(quant[dups])
    retval <- ifelse(flag, paste("[", as.character(x), "]", sep = ""), NA)

    uniqs <- unique(quant)
    # move cut points over a bit...
    reposition <- function(cut) {
      flag <- x >= cut
      if (sum(flag, na.rm = na.rm) == 0) {
        return(cut)
      } else {
        return(min(x[flag], na.rm = na.rm))
      }
    }

    newquant <- sapply(uniqs, reposition)

    retval[!flag] <- as.character(
      cut(
        x[!flag],
        breaks = newquant,
        include.lowest = TRUE,
        ...
      )
    )

    levs <- unique(retval[order(x)]) # ensure factor levels are
    # properly ordered
    retval <- factor(retval, levels = levs)

    ## determine open/closed interval ends
    mkpairs <- function(x) {
      # make table of lower, upper
      sapply(
        x,
        function(y) if (length(y) == 2) y[c(2, 2)] else y[2:3]
      )
    }
    pairs <- mkpairs(strsplit(levs, "[^0-9+\\.\\-]+"))

    rownames(pairs) <- c("lower.bound", "upper.bound")
    colnames(pairs) <- levs

    closed.lower <- rep(FALSE, ncol(pairs)) # default lower is open
    closed.upper <- rep(TRUE, ncol(pairs)) # default upper is closed
    closed.lower[1] <- TRUE # lowest interval is always closed

    for (i in 2:ncol(pairs)) {
      # open lower interval if above singlet
      if (pairs[1, i] == pairs[1, i - 1] && pairs[1, i] == pairs[2, i - 1]) {
        closed.lower[i] <- FALSE
      }
    }

    for (i in 1:(ncol(pairs) - 1)) {
      # open upper inteval if below singlet
      if (pairs[2, i] == pairs[1, i + 1] && pairs[2, i] == pairs[2, i + 1]) {
        closed.upper[i] <- FALSE
      }
    }

    levs <- ifelse(
      pairs[1, ] == pairs[2, ],
      pairs[1, ],
      paste(
        ifelse(closed.lower, "[", "("),
        pairs[1, ],
        ",",
        pairs[2, ],
        ifelse(closed.upper, "]", ")"),
        sep = ""
      )
    )
    levels(retval) <- levs
  } else {
    retval <- cut(x, quant, include.lowest = TRUE, ...)
  }

  return(retval)
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
    x = dens$x,
    y = dens$y,
    method = "linear",
    yleft = 0,
    yright = 0
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


# from the haven package
replace_with <- function(x, from, to) {
  stopifnot(length(from) == length(to))

  if (is.numeric(x)) {
    x <- as.numeric(x)
  } else if (is.character(x)) {
    x <- as.character(x)
  }

  out <- x
  # First replace regular values
  matches <- match(x, from, incomparables = NA)
  if (anyNA(matches)) {
    out[!is.na(matches)] <- to[matches[!is.na(matches)]]
  } else {
    out <- to[matches]
  }

  # Then tagged missing values
  tagged <- haven::is_tagged_na(x)
  if (!any(tagged)) {
    return(out)
  }

  matches <- match(haven::na_tag(x), haven::na_tag(from), incomparables = NA)

  # Could possibly be faster to use anyNA(matches)
  out[!is.na(matches)] <- to[matches[!is.na(matches)]]
  out
}


# standardize the data using weights
stdz <- function(x, wt = NULL) {
  # Prepare weights
  if (missing(wt)) {
    wt <- rep(1, length(x))
  }

  # calculate the weighted n
  n <- sum(wt, na.rm = TRUE)
  # center x by subtracting the mean from it
  x_mean <- x - (sum(x * wt, na.rm = TRUE) / n)
  # calculate the weighted sd
  x_sd <- sqrt(sum(wt * (x_mean)^2, na.rm = TRUE) / n)
  # divide x by the weighted sd of x to scale the data
  x <- x_mean / x_sd
  x
}

# Courtsey of Artem Sokolov from this Stack Overflow answer
# https://stackoverflow.com/questions/52066097/get-expression-that-evaluated-to-dot-in-function-called-by-magrittr-pipe/52080518#52080518
# get the name of the object piped into a function e.g., (test_data %>% funky_freqs(top)) the name is test_data
x_expression <- function(x) {
  getAST <- function(ee) purrr::map_if(as.list(ee), is.call, getAST)

  sc <- sys.calls()
  ASTs <- purrr::map(as.list(sc), getAST) %>%
    purrr::keep(~ identical(.[[1]], quote(`%>%`))) # Match first element to %>%

  if (length(ASTs) == 0) {
    return(rlang::enexpr(x))
  } # Not in a pipe
  dplyr::last(ASTs)[[2]] # Second element is the left-hand side
}


# Courtesy of Gergely Daróczi from this Stack Overflow Answer:
# https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
# determines the number of decimal places in a string
decimal_places <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[
      2
    ]])
  } else {
    return(0)
  }
}
