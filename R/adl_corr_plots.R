#' #' Create correlation plots in ADL's style
#' #'
#' #' This function allows users to create plots demonstrating the relationship
#' #' or correlation between two variabels using smoothed conditional means.
#' #' This a wrapper around [geom_smooth()] [ggplot2::geom_smooth()].
#' #'
#'
#' #'
#' #'
#' #' @param df A dataframe or tibble. This can be piped in like with a normal
#' #'   [ggplot()] [ggplot2::ggplot()] function.
#' #' @param x Variable that goes in the x-axis. This is required.
#' #' @param y Variable that goes in the y-axis. This is required.
#' #' @param color Set the grouping variable for which the confidence intervals are
#' #'   colored. This is used in stacked graphs and dodged graphs. If NULL, the
#' #'   default, no grouping variable is used. Note: No need to set this if the
#' #'   data is not grouped at all.
#' #' @param fill Set the grouping variable for which the inside of the bars are
#' #'   colored. This is used in stacked graphs and dodged graphs. If NULL, the
#' #'   default, no grouping variable is used. Note: No need to set this if the
#' #'   data is not grouped at all.
#' #' @export
#' #'
#' #'
#'
#'
#' adl_corr_plot <- function(
#'     df,
#'     x,
#'     y,
#'     add_corr_coef = FALSE,
#'     fill = NULL
#' ) {
#'
#'   # Set up x ----------------------------------------------------------------
#'
#'   # get the object's name
#'   x_lab <- deparse(substitute(x))
#'
#'   # use enexpr() to capture the expressions supplied in "x"
#'   # enexpr returns a naked expression of the argument supplied in "x"
#'   # this is what allows the input to be either a string or a symbol
#'   x <- rlang::enexpr(x)
#'   # if the object supplied in "x" is not a character...
#'   if (!is.character(x)) {
#'     # capture x and convert to a symbol object with ensym()
#'     #then use as_name() to make it a string
#'     x <- rlang::as_name(rlang::ensym(x))
#'   }
#'
#'   # get the x-axis labels
#'   if (haven::is.labelled(df[[x]])) {
#'     # if x is class haven_labelled
#'
#'     # get the x value labels as a named vector
#'     x_axis_values <- labelled::val_labels(df[[x]])
#'     # flip the named vector so we get the names
#'     x_axis_labels <- setNames(names(x_axis_values), x_axis_values) %>% stringr::str_wrap(15)
#'
#'   } else if (is.numeric(df[[x]]) && !is.null(sjlabelled::get_labels(df[[x]]))) {
#'
#'     # if x is class numeric AND DOES contain value labels
#'     # convert to a factor with sjlabelled
#'     x_axis_values <- sjlabelled::as_label(df[[x]])
#'     # flip the named vector so we get the names
#'     x_axis_labels <- setNames(names(x_axis_values), x_axis_values) %>% stringr::str_wrap(15)
#'
#'   } else if (is.numeric(df[[x]]) && is.null(sjlabelled::get_labels(df[[x]]))) {
#'
#'     x_axis_values <- unique(df[[x]]) %>% sort()
#'     x_axis_labels <- x_axis_values
#'
#'   } else {
#'
#'     # if x is character or factor retrun an error
#'     cli::cli_abort(
#'       c(
#'         "`{x_lab}` must be a vector of class {.cls haven_labelled} or {.cls numeric}",
#'         x = "You've supplied a {.cls {class(x)}} vector."
#'       )
#'     )
#'   }
#'
#'   # Set up y -------------------------------------------------------------------
#'
#'   # get the object's name
#'   y_lab <- deparse(substitute(y))
#'
#'   # use enexpr() to capture the expressions supplied in "y"
#'   # enexpr returns a naked expression of the argument supplied in "y"
#'   # this is what allows the input to be either a string or a symbol
#'   y <- rlang::enexpr(y)
#'   # if the object supplied in "y" is not a character...
#'   if (!is.character(y)) {
#'     # capture y and convert to a symbol object with ensym()
#'     #then use as_name() to make it a string
#'     y <- rlang::as_name(rlang::ensym(y))
#'   }
#'
#'   # get the y-ayis labels
#'   if (haven::is.labelled(df[[y]])) {
#'     # if y is class haven_labelled
#'
#'     # get the y value labels as a named vector
#'     y_axis_values <- labelled::val_labels(df[[y]])
#'
#'     # get the max y value
#'     y_max <- max(y_axis_values)
#'     # get the min y value
#'     y_min <- min(y_axis_values)
#'
#'     # flip the named vector so we get the names
#'     y_axis_labels <- setNames(names(y_axis_values), y_axis_values) %>% stringr::str_wrap(15)
#'
#'   } else if (is.numeric(df[[y]]) && !is.null(sjlabelled::get_labels(df[[y]]))) {
#'
#'     # if y is class numeric AND DOES contain value labels
#'     # convert to a factor with sjlabelled
#'     y_axis_values <- sjlabelled::as_label(y)
#'
#'     # get the max y value
#'     y_max <- max(y_axis_values)
#'     # get the min y value
#'     y_min <- min(y_axis_values)
#'
#'     # flip the named vector so we get the names
#'     y_axis_labels <- setNames(names(y_axis_values), y_axis_values) %>% stringr::str_wrap(15)
#'
#'   } else if (is.numeric(df[[y]]) && is.null(sjlabelled::get_labels(df[[y]]))) {
#'
#'     # get the unique values of the vector and sort it
#'     y_axis_values <- unique(df[[y]]) %>% sort()
#'
#'     # get the max y value
#'     y_max <- max(y_axis_values)
#'     # get the min y value
#'     y_min <- min(y_axis_values)
#'
#'     y_axis_labels <- y_axis_values
#'
#'   } else {
#'
#'     # if y is character or factor retrun an error
#'     cli::cli_abort(
#'       c(
#'         "`{y_lab}` must be a vector of class {.cls haven_labelled} or {.cls numeric}",
#'         y = "You've supplied a {.cls {class(y)}} vector."
#'       )
#'     )
#'   }
#'
#'   # make the graphs -----------------------------------------------------------
#'
#'   if (is.null(fill)) {
#'
#'     if (add_corr_coef == TRUE) {
#'
#'       if ("wts" %in% colnames(df)) {
#'
#'         # get the weighted correaltion if wts is present in data
#'         cor <- weights::wtd.cor(df[[x]], df[[y]], df[["wts"]])[1] %>% round(3)
#'
#'       } else {
#'
#'         # get unweighted correlation coefficient if wts is not present
#'         cor <- cor(df[[x]], df[[y]]) %>% round(3)
#'       }
#'
#'       # create the annotation grob
#'       label <- grid::textGrob(
#'         label = paste0("Correlation\nCoefficient = ", cor),
#'         x = .5, y = 0.1,
#'         hjust = 0.5,
#'         gp = grid::gpar(col = "black",size = 3, family = "L")
#'       )
#'
#'       # create the plot
#'       plot <- df %>%
#'         ggplot2::ggplot(ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
#'         # add the weights in the aes
#'         ggplot2::geom_smooth(
#'           aes(weight = wts),
#'           # adjust color and fill
#'           method = "lm", color = "#00A0E0FF", fill = "#00A0E0FF"
#'         ) +
#'         theme_default() +
#'         ggplot2::annotation_custom(label, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
#'         ggplot2::labs(
#'           x = labelled::var_label(df[[x]]),
#'           y = labelled::var_label(df[[y]])
#'         ) +
#'         ggplot2::scale_y_continuous(
#'           limits = c(y_min, y_max),
#'           breaks = y_axis_values,
#'           labels = y_axis_labels
#'         ) +
#'         ggplot2::scale_x_continuous(
#'           breaks = x_axis_values,
#'           labels = x_axis_labels
#'         )
#'
#'       return(plot)
#'
#'     } else{
#'
#'       # create the plot
#'       plot <- df %>%
#'         ggplot2::ggplot(ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
#'         # add the weights in the aes
#'         ggplot2::geom_smooth(
#'           aes(weight = wts),
#'           # adjust color and fill
#'           method = "lm", color = "#00A0E0FF", fill = "#00A0E0FF"
#'         ) +
#'         theme_default() +
#'         ggplot2::labs(
#'           x = labelled::var_label(df[[x]]),
#'           y = labelled::var_label(df[[y]])
#'         ) +
#'         ggplot2::scale_y_continuous(
#'           limits = c(y_min, y_max),
#'           breaks = y_axis_values,
#'           labels = y_axis_labels
#'         ) +
#'         ggplot2::scale_x_continuous(
#'           breaks = x_axis_values,
#'           labels = x_axis_labels
#'         )
#'
#'       return(plot)
#'
#'     }
#'
#'   }
#'
#'
#'
#'
#' }
#'
#'
#'
#'
#'
#' pol_pos
#' adl_corr_plot(pol_pos, trad_n, power)
#'
#' pol_pos %>%
#'   ggplot(aes(x = acts_avg, y = trad_n)) +
#'   geom_smooth(
#'     method = "lm",
#'     fill = adl_palettes$primary, color = adl_palettes$primary
#'   ) +
#'   geom_jitter(
#'     shape = 16,
#'     color = adl_palettes$primary,
#'     alpha = 0.5,
#'     width = 0.5,
#'     height = 0.5,
#'     data = ~.x %>% sample_frac(0.1)
#'   ) +
#'   xlim(c(1,4)) +
#'   theme_default()
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
