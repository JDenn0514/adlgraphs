#' #' Create ridgeline plot
#' #'
#' #' `geom_density_ridges` arranges multiple density plots in a staggered fashion, as in the cover of the famous Joy Division album Unknown Pleasures.
#' #'
#' #' By default, this geom calculates densities from the point data mapped onto the x axis. If density calculation is
#' #' not wanted, use `stat="identity"` or use [`geom_ridgeline`]. The difference between `geom_density_ridges` and [`geom_ridgeline`]
#' #' is that `geom_density_ridges` will provide automatic scaling of the ridgelines (controlled by the `scale` aesthetic), whereas
#' #' [geom_ridgeline] will plot the data as is. Note that when you set `stat="identity"`, the `height` aesthetic must
#' #' be provided.
#' #'
#' #' Note that the default [`stat_density_ridges`] makes joint density estimation across all datasets. This may not generate
#' #' the desired result when using faceted plots. As an alternative, you can set `stat = "density"` to use [`stat_density`].
#' #' In this case, it is required to add the aesthetic mapping `height = after_stat(density)` (see examples).
#' #'
#' #' @param panel_scaling If `TRUE`, the default, relative scaling is calculated separately
#' #' for each panel. If `FALSE`, relative scaling is calculated globally.
#' #' @inheritParams geom_ridgeline
#' #'
#' #' @section Aesthetics:
#' #'
#' #' Required aesthetics are in bold.
#' #'
#' #' * **`x`**
#' #' * **`y`**
#' #' * `weight` Optional case weights passed to `stats::density` to calculate a weighted density estimate
#' #' * `group` Defines the grouping. Not needed if a categorical variable is mapped onto `y`, but needed otherwise. Will typically be the same
#' #' variable as is mapped to `y`.
#' #' * `height` The height of each ridgeline at the respective x value. Automatically calculated and
#' #' provided by [`stat_density_ridges`] if the default stat is not changed.
#' #' * `scale` A scaling factor to scale the height of the ridgelines relative to the spacing between them.
#' #' A value of 1 indicates that the maximum point of any ridgeline touches the baseline right above, assuming
#' #' even spacing between baselines.
#' #' * `rel_min_height` Lines with heights below this cutoff will be removed. The cutoff is measured relative to the
#' #' overall maximum, so `rel_min_height=0.01` would remove everything that is 1\% or less than the highest point among all
#' #' ridgelines. Default is 0, so nothing is removed.
#' #' alpha
#' #' * `colour`, `fill`, `group`, `alpha`, `linetype`, `linewidth`, as in [`geom_ridgeline`].
#' #' * `point_shape`, `point_colour`, `point_size`, `point_fill`, `point_alpha`, `point_stroke`, as in [`geom_ridgeline`].
#' #'
#' #' @importFrom ggplot2 layer
#' #' @export
#' #' @examples
#' #' library(ggplot2)
#' #'
#' #' # set the `rel_min_height` argument to remove tails
#' #' ggplot(iris, aes(x = Sepal.Length, y = Species)) +
#' #'   geom_density_ridges(rel_min_height = 0.005) +
#' #'   scale_y_discrete(expand = c(0.01, 0)) +
#' #'   scale_x_continuous(expand = c(0.01, 0)) +
#' #'   theme_ridges()
#' #'
#' #' # set the `scale` to determine how much overlap there is among the plots
#' #' ggplot(diamonds, aes(x = price, y = cut)) +
#' #'   geom_density_ridges(scale = 4) +
#' #'   scale_y_discrete(expand = c(0.01, 0)) +
#' #'   scale_x_continuous(expand = c(0.01, 0)) +
#' #'   theme_ridges()
#' #'
#' #' # the same figure with colors, and using the ggplot2 density stat
#' #' ggplot(diamonds, aes(x = price, y = cut, fill = cut, height = after_stat(density))) +
#' #'   geom_density_ridges(scale = 4, stat = "density") +
#' #'   scale_y_discrete(expand = c(0.01, 0)) +
#' #'   scale_x_continuous(expand = c(0.01, 0)) +
#' #'   scale_fill_brewer(palette = 4) +
#' #'   theme_ridges() + theme(legend.position = "none")
#' geom_density_ridges <- function(mapping = NULL, data = NULL, stat = "density_ridges",
#'                                 position = "points_sina", panel_scaling = TRUE,
#'                                 na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
#'   layer(
#'     data = data,
#'     mapping = mapping,
#'     stat = stat,
#'     geom = GeomDensityRidges,
#'     position = position,
#'     show.legend = show.legend,
#'     inherit.aes = inherit.aes,
#'     params = list(
#'       na.rm = na.rm,
#'       panel_scaling = panel_scaling,
#'       ...
#'     )
#'   )
#' }
#'
#' #' @rdname geom_density_ridges
#' #' @format NULL
#' #' @usage NULL
#' #' @importFrom grid gTree gList
#' #' @export
#' GeomDensityQuant <- ggproto(
#'   "GeomDensityQuant",
#'   GeomRidgeline,
#'   default_aes = aes(
#'     # ridgeline aesthetics
#'     color = "black", fill = "grey70", linewidth = 0.5, linetype = 1,
#'     rel_min_height = 0, scale = 1.8, alpha = NA, datatype = "ridgeline",
#'
#'     # point aesthetics with default
#'     point_shape = 19, point_size = 1.5, point_stroke = 0.5,
#'
#'     # point aesthetics, inherited
#'     point_colour = NULL, #point_color = NULL,
#'     point_fill = NULL, point_alpha = NULL,
#'
#'     # vline aesthetics, all inherited
#'     vline_colour = NULL, #vline_color = NULL,
#'     vline_width = NULL, vline_linetype = NULL,
#'     vline_size = NULL #<- line size deprecated in ggplot2 3.4.0
#'   ),
#'
#'   required_aes = c("x", "y", "height"),
#'
#'   optional_aes = c("point_color", "vline_color", "vline_size", "vline_width", "weight"),
#'
#'   extra_params = c("na.rm", "panel_scaling"),
#'
#'   setup_data = function(self, data, params) {
#'
#'     # check for size deprecation
#'     params <- check_vline_size_param(params)
#'     params <- check_size(params)
#'
#'     # provide default for panel scaling parameter if it doesn't exist,
#'     # happens if the geom is called from a stat
#'     if (is.null(params$panel_scaling)) {
#'       params$panel_scaling <- TRUE
#'     }
#'
#'     # calculate internal scale
#'     yrange = max(data$y) - min(data$y)
#'     n = length(unique(data$y))
#'     if (n<2) {
#'       hmax <- max(data$height, na.rm = TRUE)
#'       iscale <- 1
#'     }
#'     else {
#'       # scale per panel or globally?
#'       if (params$panel_scaling) {
#'         heights <- split(data$height, data$PANEL)
#'         max_heights <- vapply(heights, max, numeric(1), na.rm = TRUE)
#'         hmax <- max_heights[data$PANEL]
#'         iscale <- yrange/((n-1)*hmax)
#'       }
#'       else {
#'         hmax <- max(data$height, na.rm = TRUE)
#'         iscale <- yrange/((n-1)*hmax)
#'       }
#'     }
#'
#'     #print(iscale)
#'     #print(hmax)
#'
#'     data <- cbind(data, iscale)
#'
#'     if (!"scale" %in% names(data)) {
#'       if (!"scale" %in% names(params))
#'         data <- cbind(data, scale = self$default_aes$scale)
#'       else
#'         data <- cbind(data, scale = params$scale)
#'     }
#'
#'     if (!"rel_min_height" %in% names(data)){
#'
#'       if (!"rel_min_height" %in% names(params))
#'         data <- cbind(data, rel_min_height = self$default_aes$rel_min_height)
#'       else
#'         data <- cbind(data, rel_min_height = params$rel_min_height)
#'     }
#'
#'     # warn for vline_size or size arg
#'     data <- check_vline_size(data)
#'     data <- check_size(data)
#'
#'     transform(data,
#'               ymin = y,
#'               ymax = y + iscale*scale*height,
#'               min_height = hmax*rel_min_height)
#'   }
#' )
