# Code for stat_density_ridges based on stat_density_common in the "extending ggplot2" vignette

#' Stat for density ridgeline plots
#'
#' This stat is the default stat that will be used in `geom_density_quant` when
#' I get around to making it. Nevertheless, it still works with
#' \code{\link[ggplot2]{geom_density}}. It is very similar to
#' \code{\link[ggplot2]{stat_density}} and
#' \code{\link[ggridges]{stat_density_ridges}} as it was built as a sort of
#' combination of the two. One of the key differences between this function and
#' those two is that this one uses the Sheather & Jones ("sj") as the default bandwidth
#' selector. This is done because this is a better bandwidth selector than
#' Silverman's ("nrd0") which is the default for the other two functions. In
#' addition, this function allows you to add quantile lines similar to
#' \code{\link[ggridges]{stat_density_ridges}}.
#'
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and
#'   `inherit.aes = TRUE` (the default), it is combined with the default mapping
#'    at the top level of the plot. You must supply `mapping` if there is no
#'    plot mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot()].
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    [fortify()] for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame`, and
#'    will be used as the layer data. A `function` can be created
#'    from a `formula` (e.g. `~ head(.x, 10)`).
#' @param geom The geometric object to use to display the data. [`geom_density`]
#'   is the default.
#' @param position A position adjustment to use on the data for this layer. This
#'   can be used in various ways, including to prevent overplotting and
#'   improving the display. The `position` argument accepts the following:
#'   * The result of calling a position function, such as `position_jitter()`.
#'     This method allows for passing extra arguments to the position.
#'   * A string naming the position adjustment. To give the position as a
#'     string, strip the function name of the `position_` prefix. For example,
#'     to use `position_jitter()`, give the position as `"jitter"`.
#'   * For more information and other ways to specify the position, see the
#'     [layer position][layer_positions] documentation.
#' @param ... Other arguments passed on to [layer()]'s `params` argument. These
#'   arguments broadly fall into one of 4 categories below. Notably, further
#'   arguments to the `position` argument, or aesthetics that are required
#'   can *not* be passed through `...`. Unknown arguments that are not part
#'   of the 4 categories below are ignored.
#'   * Static aesthetics that are not mapped to a scale, but are at a fixed
#'     value and apply to the layer as a whole. For example, `colour = "red"`
#'     or `linewidth = 3`. The geom's documentation has an **Aesthetics**
#'     section that lists the available options. The 'required' aesthetics
#'     cannot be passed on to the `params`. Please note that while passing
#'     unmapped aesthetics as vectors is technically possible, the order and
#'     required length is not guaranteed to be parallel to the input data.
#'   * When constructing a layer using
#'     a `stat_*()` function, the `...` argument can be used to pass on
#'     parameters to the `geom` part of the layer. An example of this is
#'     `stat_density(geom = "area", outline.type = "both")`. The geom's
#'     documentation lists which parameters it can accept.
#'   * Inversely, when constructing a layer using a
#'     `geom_*()` function, the `...` argument can be used to pass on parameters
#'     to the `stat` part of the layer. An example of this is
#'     `geom_area(stat = "density", adjust = 0.5)`. The stat's documentation
#'     lists which parameters it can accept.
#'   * The `key_glyph` argument of [`layer()`] may also be passed on through
#'     `...`. This can be one of the functions described as
#'     [key glyphs][draw_key], to change the display of the layer in the legend.
#' @param bw The smoothing bandwidth to be used.
#'   If numeric, the standard deviation of the smoothing kernel.
#'   If character, a rule to choose the bandwidth, as listed in
#'   [stats::bw.nrd()]. Note that automatic calculation of the bandwidth does
#'   not take weights into account. Default is `sj`.
#' @param adjust A multiplicate bandwidth adjustment. This makes it possible
#'    to adjust the bandwidth while still using the a bandwidth estimator.
#'    For example, `adjust = 1/2` means use half of the default bandwidth.
#' @param kernel Kernel. See list of available kernels in [density()].
#' @param n number of equally spaced points at which the density is to be
#'   estimated, should be a power of two, see [density()] for
#'   details
#' @param na.rm If `FALSE` (the default), removes missing values with
#'    a warning.  If `TRUE` silently removes missing values.
#' @param bounds Known lower and upper bounds for estimated data. Default
#'   `c(-Inf, Inf)` means that there are no (finite) bounds. If any bound is
#'   finite, boundary effect of default density estimation will be corrected by
#'   reflecting tails outside `bounds` around their closest edge. Data points
#'   outside of bounds are removed with a warning.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. [borders()].
#' @param quantile_lines Logical. Determines if quantile lines should be drawn
#'   or not. FALSE is default.
#' @param calc_ecdf If `TRUE`, `stat_density_ridges` calculates an empirical
#'   cumulative distribution function (ecdf) and returns a variable `ecdf` and
#'   a variable `quantile`. Both can be mapped onto aesthetics via `stat(ecdf)`
#'   and `stat(quantile)`, respectively.
#' @param quantiles Sets the number of quantiles the data should be broken into.
#'   Used if either `calc_ecdf = TRUE` or `quantile_lines = TRUE`. If
#'   `quantiles` is an integer then the data will be cut into that many equal
#'   quantiles. If it is a vector of probabilities then the data will cut by them.
#' @export
stat_density_quant <- function(mapping = NULL, data = NULL,
                               geom = geom, position = "stack",
                               ...,
                               bw = "sj",
                               adjust = 1,
                               kernel = "gaussian",
                               n = 512,
                               na.rm = FALSE,
                               bounds = c(-Inf, Inf),
                               show.legend = NA,
                               inherit.aes = TRUE,
                               quantile_lines = FALSE,
                               calc_ecdf = FALSE,
                               quantiles = 4) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatDensityQuant,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      na.rm = na.rm,
      bounds = bounds,
      calc_ecdf = calc_ecdf,
      quantiles = quantiles,
      quantile_lines = quantile_lines,
      ...
    )
  )
}




StatDensityQuant <- ggplot2::ggproto(
  "StatDensityQuant",
  Stat,
  required_aes = "x",

  default_aes = aes(x = ggplot2::after_stat(density), y = ggplot2::after_stat(density), fill = NA, weight = NULL),

  dropped_aes = "weight",

  calc_panel_params = function(data, params) {
    if (is.null(params$bandwidth)) {
      xdata <- stats::na.omit(data.frame(x=data$x, group=data$group))
      xs <- split(xdata$x, xdata$group)
      xs_mask <- vapply(xs, length, numeric(1)) > 1
      bws <- vapply(xs[xs_mask], bw.nrd0, numeric(1))
      bw <- mean(bws, na.rm = TRUE)
      message("Picking joint bandwidth of ", signif(bw, 3))

      params$bandwidth <- bw
    }

    if (is.null(params$from)) {
      params$from <- min(data$x, na.rm=TRUE) - 3 * params$bandwidth
    }

    if (is.null(params$to)) {
      params$to <- max(data$x, na.rm=TRUE) + 3 * params$bandwidth
    }

    data.frame(
      bandwidth = params$bandwidth,
      from = params$from,
      to = params$to
    )
  },

  setup_params = function(self, data, params) {

    # determine if there is x
    has_x <- !(is.null(data$x) && is.null(params$x))
    # determine if there is y
    has_y <- !(is.null(data$y) && is.null(params$y))
    # if neither exists give an error
    if (!has_x && !has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} requires an {.field x} or {.field y} aesthetic.")
    }

    # calculate bandwidth, min, and max for each panel separately
    panels <- split(data, data$PANEL)
    pardata <- lapply(panels, self$calc_panel_params, params)
    pardata <- purrr::reduce(pardata, rbind)

    if (length(params$quantiles) > 1 &&
        (max(params$quantiles, na.rm = TRUE) > 1 || min(params$quantiles, na.rm = TRUE) < 0)) {
      stop('invalid quantiles used: c(', paste0(params$quantiles, collapse = ','), ') must be within [0, 1] range')
    }

    params$bandwidth <- pardata$bandwidth
    params$from <- pardata$from
    params$to <- pardata$to
    params
  },

  compute_group = function(data, scales, bw = "sj", adjust = 1, kernel = "gaussian",
                           n = 512, na.rm = FALSE, bounds = c(-Inf, Inf),
                           calc_ecdf = FALSE, quantile_lines = FALSE, quantiles = 4
  ) {


    # ignore too small groups
    if(nrow(data) < 3) return(data.frame())

    range <- range(data$x, na.rm = TRUE)

    nx <- length(data$x)


    # when quantile lines are requested, we also calculate ecdf this simplifies
    # things for now; in principle, could disentangle the two
    if (quantile_lines) calc_ecdf <- TRUE

    weights <- rep(1 / nx, nx)

    # calculate the density
    if (any(is.finite(bounds))) {
      # if any of the values in bounds are finite

      # get the data within the bounds
      sample_data <- fit_data_to_bounds(bounds, data$x, weights)

      # calculate the density
      d <- stats::density(
        sample_data$x,
        weights = sample_data$w,
        bw = bw,
        adjust = adjust,
        kernel = kernel,
        n = n
      )

      # Update density estimation to mitigate boundary effect at known `bounds`
      dens <- reflect_density(
        dens = d,
        bounds = bounds,
        from = range[1],
        to = range[2]
      )

    } else {
      # if bounds = c(-Inf, Inf)

      d <- stats::density(
        data$x,
        weights = weights,
        bw = bw,
        adjust = adjust,
        kernel = kernel,
        n = n,
        from = range[1],
        to = range[2]
      )

    }

    # calculate maximum density for scaling
    maxdens <- max(d$y, na.rm = TRUE)

    # make interpolating function for density line
    densf <- stats::approxfun(d$x, d$y, rule = 2)


    # calculate quantiles, needed for both quantile lines and ecdf
    if ((length(quantiles)==1) && (all(quantiles >= 1))) {
      if (quantiles > 1) {
        probs <- seq(0, 1, length.out = quantiles + 1)[2:quantiles]
      }
      else {
        probs <- NA
      }
    } else {
      probs <- quantiles
      probs[probs < 0 | probs > 1] <- NA
    }
    qx <- stats::na.omit(stats::quantile(data$x, probs = probs))

    # if requested, add data frame for quantile lines
    df_quantiles <- NULL

    if (quantile_lines && length(qx) > 0) {
      qy <- densf(qx)
      df_quantiles <- data.frame(
        x = qx,
        density = qy,
        ndensity = qy / maxdens,
        scaled = qy / maxdens,
        count = qy * nx,
        n = nx,
        datatype = "vline",
        stringsAsFactors = FALSE
      )

    }

    if (calc_ecdf) {
      n <- length(d$x)
      ecdf <- c(0, cumsum(d$y[1:(n-1)]*(d$x[2:n]-d$x[1:(n-1)])))
      ecdf_fun <- stats::approxfun(d$x, ecdf, rule = 2)
      ntile <- findInterval(d$x, qx, left.open = TRUE) + 1 # if make changes here, make them also below

      if (!is.null(df_quantiles)) {
        # we add data for ecdf and quantiles back to all other data points
        df_quantiles <- data.frame(
          df_quantiles,
          ecdf = ecdf_fun(df_quantiles$x),
          quantile = findInterval(df_quantiles$x, qx, left.open = TRUE) + 1
        )
      }

      df_density <- data.frame(
        x = d$x,
        density = d$y,
        ndensity = d$y / maxdens,
        scaled =  d$y / maxdens,
        count =   d$y * nx,
        n = nx,
        ecdf = ecdf,
        quantile = ntile,
        datatype = "ridgeline",
        stringsAsFactors = FALSE
      )
    }
    else {
      df_density <- data.frame(
        x = d$x,
        density = d$y,
        ndensity = d$y / maxdens,
        scaled =  d$y / maxdens,
        count =   d$y * nx,
        n = nx,
        ecdf = ecdf,
        quantile = ntile,
        datatype = "ridgeline",
        stringsAsFactors = FALSE
      )
    }

    df_final <- rbind(df_density, df_quantiles)
    # now combine everything and turn quantiles into factor
    if ("quantile" %in% names(df_final)) {
      df_final$quantile <- factor(df_final$quantile)
    }

    df_final
  }

)

