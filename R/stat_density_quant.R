# Code for stat_density_ridges based on stat_density_common in the "extending ggplot2" vignette

#' Stat for density ridgeline plots
#'
#' This stat is the default stat used by [`geom_density_quant`]. It is very
#' similar to \code{\link[ggplot2]{stat_density}} and
#' \code{\link[ggridges]{stat_density_ridges}} as it was built as a sort of
#' combination of the two. One of the key differences between this function and
#' those two is that this one uses the Sheather & Jones ("sj") as the default bandwidth
#' selector. This is done because this is a better bandwidth selector than
#' Silverman's ("nrd0") which is the default for the other two functions. In
#' addition, this function allows you to add quantile lines similar to
#' \code{\link[ggridges]{stat_density_ridges}}.
#'
#' @param geom The geometric object to use to display the data. [`geom_density_quant`]
#'   is the default.
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
#' @param bounds Known lower and upper bounds for estimated data. Default
#'   `c(-Inf, Inf)` means that there are no (finite) bounds. If any bound is
#'   finite, boundary effect of default density estimation will be corrected by
#'   reflecting tails outside `bounds` around their closest edge. Data points
#'   outside of bounds are removed with a warning.
#' @param from,to The left and right-most points of the grid at which the density is to be estimated,
#'   as in [`density()`]. If not provided, these are estimated from the data range and the bandwidth.
#'   and sets it to `TRUE`.
#' @param calc_ecdf If `TRUE`, `stat_density_ridges` calculates an empirical
#'   cumulative distribution function (ecdf) and returns a variable `ecdf` and
#'   a variable `quantile`. Both can be mapped onto aesthetics via `stat(ecdf)`
#'   and `stat(quantile)`, respectively.
#' @param quantiles Sets the number of quantiles the data should be broken into.
#'   Used if either `calc_ecdf = TRUE` or `quantile_lines = TRUE`. If
#'   `quantiles` is an integer then the data will be cut into that many equal
#'   quantiles. If it is a vector of probabilities then the data will cut by them.
#' @param quantile_fun Function that calculates quantiles. The function needs
#'   to accept two parameters, a vector `x` holding the raw data values and a
#'   vector `probs` providing the probabilities that define the quantiles.
#'   Default is `quantile`.
#' @param n The number of equally spaced points at which the density is to be
#'   estimated. Should be a power of 2. Default is 512.
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
                               quantiles = 4,
                               quantile_fun = quantile) {

  layer(
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
      quantile_fun = quantile_fun,
      ...
    )
  )
}




StatDensityQuant <- ggproto(
  "StatDensityQuant",
  Stat,
  required_aes = "x",

  default_aes = aes(x = after_stat(density), y = after_stat(density), fill = NA, weight = NULL),

  dropped_aes = "weight",

  calc_panel_params = function(data, params) {
    if (is.null(params$bandwidth)) {
      xdata <- na.omit(data.frame(x=data$x, group=data$group))
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
    # determine if the plot is flipped
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE, main_is_continuous = TRUE)

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
    pardata <- reduce(pardata, rbind)

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
                           calc_ecdf = FALSE, quantile_lines = FALSE, quantiles = 4,
                           quantile_fun = quantile
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
      sample_data <- ggplot2:::fit_data_to_bounds(bounds, data$x, weights)

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
      dens <- ggplot2:::reflect_density(
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
    densf <- approxfun(d$x, d$y, rule = 2)


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
    qx <- na.omit(quantile_fun(data$x, probs = probs))

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
      ecdf_fun <- approxfun(d$x, ecdf, rule = 2)
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

