#' Create bar plots in ADL's style
#'
#' This function allows users to create different types of bar plots with
#' ADL's style. This a wrapper around \code{\link[ggplot2]{geom_col}},
#' \code{\link[ggplot2]{geom_text}}/,\code{\link[ggplot2]{geom_label}}, and
#' \code{\link[ggplot2]{geom_errorbar}}. This function was created to
#' standardize the graphs produced by CAR's team and to cut down on the amount
#' of time it takes to make these graphs.
#'
#' As mentioned previously, this function is a wrapper around various `{ggplot2}`
#' functions in order to save time when making simple bar plots, dodged bar
#' plots, and stacked bar plots. As a result, it is not possible to combine
#' every element of the graph. If you would like to do so, we recommend using the
#' actual geoms from `{ggplot2}`.
#'
#' Each of the elements included serve a purpose to allow you to customize the
#' graphs so that they look nice. Moreover, the arguments were created for the
#' types of graphs that CAR produces, namely mean plots and frequency plots.
#'
#'
#' @param df A dataframe or tibble. This can be piped in like with a normal
#'   \code{\link[ggplot2]{ggplot}} function.
#' @param x Variable that goes in the x-axis. This is required.
#' @param y Variable that goes in the y-axis. This is required.
#' @param color Set the grouping variable for which the confidence intervals are
#'   colored. This is used in stacked graphs and dodged graphs. If NULL, the
#'   default, no grouping variable is used. Note: No need to set this if the
#'   data is not grouped at all.
#' @param fill Set the grouping variable for which the inside of the bars are
#'   colored. This is used in stacked graphs and dodged graphs. If NULL, the
#'   default, no grouping variable is used. Note: No need to set this if the
#'   data is not grouped at all.
#' @export
#'
#'


# adl_corr_plot <- function(
#     df,
#     x,
#     y,
#     color = NULL,
#     fill = NULL,
#     wt = NULL) {
#
#   # get
#   values <- val_labels(df[[var]])
#   value_labels <- setNames(names(values), values) %>% str_wrap(15)
#
#   # get correlation coefficient
#   if (is.null(wt)) {
#     cor <- wtd.cor(df[[x]], df[[y]], df[[wts]]) %>%
#       # convert to a tibble
#       as_tibble() %>%
#       mutate(cor = round(correlation, 3)) %>%
#       select(cor)
#
#   } else
#   cor <- wtd.cor(df[[x]], df[[y]], df[[wts]]) %>%
#     # convert to a tibble
#     as_tibble() %>%
#     mutate(cor = round(correlation, 3)) %>%
#     select(cor)
#
#   label <- textGrob(
#     label = paste("Correlation\nCoefficient = ", cor$cor),
#     x = .5, y = 0.1,
#     hjust = 0.5,
#     gp=gpar(col = "black",size = 3, family = "L")
#   )
#
#   # create the plot
#   df %>%
#     ggplot(., aes(x = !!sym({{ var }}), y = trad_avg_n)) +
#     # add the weights in the aes
#     geom_smooth(aes(weight = wts),
#                 # adjust color and fill
#                 method = "lm", color = "#9478d2", fill = "#9478d2") +
#     scale_x_continuous(labels = value_labels) +
#     # add titles
#     labs(x = str_wrap(x_title, 45),
#          y = "Antisemitism Score",
#          #title = glue("{group} ({var})"),
#          title = paste0("African Americans (", var, ")")) +
#     theme_default() +
#     #coord_cartesian(ylim = c(1.5, 4)) +
#     annotation_custom(label, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
# }
