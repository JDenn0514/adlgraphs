#### THIS CODE IS HEAVILY INSPIRED BY CODE FROM cmapplot SO SHOUT OUT THEM FOR THE HELP

# Initialize environment for adlgraphs global variables
adlgraphs_global <- new.env(parent = emptyenv())

# Establish names of preferred fonts
adlgraphs_global$preferred_font <- list(
  "heavy" = "Roboto Heavy",
  "bold" = "Roboto Bold",
  "regular" = "Roboto Regular",
  "light" = "Roboto Light"
)

# Set up default font handling
# (Note: overridden by .onLoad() if Whitney is available)
adlgraphs_global$use_roboto <- FALSE
adlgraphs_global$font <- list(
  heavy = list(family = "sans", face = "bold"),
  bold = list(family = "sans", face = "bold"),
  regular = list(family = "sans", face = "plain"),
  light = list(family = "sans", face = "plain")
)


# Set common font sizes (pts)
adlgraphs_global$fsize <- list(
  S = 9.6,
  M = 12,
  L = 14.4
)

# Define CMAP colors
adlgraphs_global$colors <- list(
  blackish = "#222222"
)


#' # Define CMAP palettes
#' adlgraphs_global$palettes <- tibble::tribble(
#'   ~name, ~type, ~colors,
#'
#'   # individual colors
#'   "primary",   "single_color", "#14A2FC",
#'   "secondary", "single_color", "#B0B1B3",
#'   "tertiary",  "single_color", "#E84C4C",
#'
#'   # discrete
#'   "categorical", "discrete", c("#14A2FC", "#B0B1B3", "#E84C4C", "#0A1A50", "#FFE500", "#69DA78", "#60269e", "#FFA828"),
#'   "pid3",        "discrete", c("#14A2FC", "#60269e", "#E84C4C"),
#'
#'
#'   # Single-hue sequential
#'   "bluescale",      "sequential", c("#0A1A50","#E7E8EE"),
#'
#'   # Multi-hue divergent
#'   "likert_4", "divergent", c("#0A1A50", "#14A2FC", "#DBDCDD", "#595b60"),
#'   "likert_6", "divergent", c("#0A1A50", "#005B98", "#14A2FC", "#DBDCDD","#B0B1B3", "#595b60")
#' )
#'
#' # Establish plotting constants in bigpts (1/72 of inch)
#' adlgraphs_global$consts <- list(
#'   lwd_gridline = 0.3,
#'   lwd_strongline = 1,
#'   lwd_plotline = 3,
#'   lwd_topline = 2,
#'   length_ticks = 7,
#'   margin_topline_t = 5,
#'   margin_title_t = 5,
#'   margin_title_b = 5,
#'   margin_caption_b = 5,
#'   margin_legend_t = 5,
#'   margin_legend_i = 8,
#'   margin_legend_b = 10,
#'   margin_plot_b = 5,
#'   margin_sidebar_l = 2,
#'   margin_plot_l = 10,
#'   margin_plot_r = 10,
#'   margin_panel_r = 10,
#'   leading_title = 1,
#'   leading_caption = 1
#' )
#'
#' # List of all geoms whose aesthetics will be modified by cmapplot
#' adlgraphs_global$geoms_that_change <- c(
#'   "Label",
#'   "Line",
#'   "Text",
#'   "TextLast",
#'   "PointLast",
#'   "RecessionsText"
#' )
#'
#'
#' #'The adlgraphs_global environment
#' #'
#' #'The \code{adlgraphs_global} environment contains a list of predefined
#' #'variables for use by the cmapplot package and its users. It includes commonly
#' #'used colors, font and font size specifications, and a list of constants which
#' #'aid in drawing cmap-themed plots. It cannot be accessed directly, but the
#' #'helper functions described here provide the user access if needed.
#' #'
#' #'@section Plot Constants: The primary portion of these global variables of
#' #'  interest to the user is \code{adlgraphs_global$consts}, a list of default
#' #'  constants that set certain plot aesthetics. Units of all plot constants are
#' #'  "bigpts": 1/72 of an inch. Most plot constants are invoked (and can be
#' #'  overridden) in \code{\link{finalize_plot}}: these are marked below with an
#' #'  \strong{F}. Some are used/can be overridden in \code{\link{theme_cmap}}:
#' #'  these are marked with \strong{T}.
#' #'
#' #'  \itemize{ \item \code{lwd_strongline}: This stronger-width line is drawn
#' #'  vertically or horizontally with the \code{hline, vline} args of
#' #'  \code{theme_cmap()}. \strong{(T)} \item \code{lwd_gridline}: This
#' #'  thinner-width line is drawn vertically or horizontally with the
#' #'  \code{gridlines, axislines} args of \code{theme_cmap()}. \strong{(T)} \item
#' #'  \code{lwd_plotline}: The width of any lines drawn by geoms in the plot (e.g.
#' #'  \code{geom_line}) but not explicitly sized by the geom's aesthetic.
#' #'  Implemented by \code{finalize_plot} or by \code{apply_cmap_default_aes} but
#' #'  not overridable in either context. (Modify by setting the size explicitly in
#' #'  the geom, but see \code{gg_lwd_convert} first.) \item \code{lwd_topline}:
#' #'  The width of the line above the plot. \strong{(F)} \item
#' #'  \code{length_ticks}: The length of the axis ticks (if shown). \strong{(T)}
#' #'  \item \code{margin_topline_t}: The margin between the top edge of the image
#' #'  and the top line. \strong{(F)} \item \code{margin_title_t}: The margin
#' #'  between the top line and the title. \strong{(F)} \item
#' #'  \code{margin_title_b}: The margin between the title and the caption when
#' #'  both are drawn in the sidebar. \strong{(F)} \item \code{margin_caption_b}:
#' #'  The margin between the bottom of the caption and the bottom edge of the
#' #'  image. \strong{(F)} \item \code{margin_legend_t}: The margin between the top
#' #'  line and the plot box (i.e., the top of the legend). \strong{(F)} \item
#' #'  \code{margin_legend_i}: The margin between legends (this only applies in
#' #'  plots with two or more legends and does not affect legend spacing on plots
#' #'  with single legends that have multiple rows). \strong{(T, F)} \item
#' #'  \code{margin_legend_b}: The margin between the bottom of the legend and the
#' #'  rest of the plot. \strong{(T, F)} \item \code{margin_plot_b}: The margin
#' #'  between the bottom of the plot and the bottom edge of the image (or top of
#' #'  caption). \strong{(F)} \item \code{margin_sidebar_l}: The margin between the
#' #'  left edge of the image and the title and caption, when the sidebar exists.
#' #'  Deducted from \code{title_width}. \strong{(F)} \item \code{margin_plot_l}:
#' #'  The margin between the left edge of the plot and the sodebar. \strong{(F)}
#' #'  \item \code{margin_plot_r}: The margin between the right edge of the plot
#' #'  and the edge of the image. \strong{(F)} \item \code{margin_panel_r}: Padding
#' #'  between the plot and its right-hand drawing extent. Override this based on
#' #'  space needed for x axis labels. \strong{(T)} \item \code{leading_title}:
#' #'  Text leading for Title text. \strong{(F)} \item \code{leading_caption}: Text
#' #'  leading for Caption text. \strong{(F)} }
#' #'
#' #' @aliases adlgraphs_global
#' #'
#' #' @describeIn get_adlgraphs_global Get the entire environment as a list.
#' #'
#' #' @export
#' get_adlgraphs_global <- function(){
#'   as.list(adlgraphs_global)
#' }
#'
#' #' Get a value from the adlgraphs_global environment
#' #'
#' #' @examples
#' #'
#' #' # These are the same:
#' #' get_cmapplot_global("consts$lwd_gridline")
#' #' get_cmapplot_global("consts", "lwd_gridline")
#' #'
#' #' @describeIn get_adlgraphs_global Get a specific global value
#' #'
#' #' @export
#' get_cmapplot_global <- function(...){
#'
#'   # establish vector of sublocations
#'   names <- unlist(stringr::str_split(c(...), "\\$"))
#'
#'   # fetch the top-level element from the list
#'   var <- get(names[1], envir = adlgraphs_global)
#'
#'   # recurse over additional names to extract the right value
#'   for(i in seq_along(names[-1])+1){
#'     var <- var[[names[i]]]
#'   }
#'
#'   if(is.null(var)){
#'     stop(paste0("object '", paste(names, collapse = "$"), "' not found"))
#'   }
#'
#'   return(var)
#'
#' }
#'
#'
#' #' Set a value in the adlgraphs_global environment
#' #'
#' #' @param value the value to be set
#' #'
#' #' @param ... The path to the variable within \code{adlgraphs_global} to be
#' #'   get/set. The function willparse \code{$}, or recursive list elements can be
#' #'   split over multiple arguments (e.g. \code{"font$strong$family"} is
#' #'   equivalent to \code{"font", "strong", "family"}).
#' #'
#' #' @param quietly suppress confirmatory messages
#' #'
#' #' @examples
#' #'
#' #' # Globals can be modified if needed
#' #' set_cmapplot_global(5, "consts$lwd_gridline")
#' #' get_cmapplot_global("consts$lwd_gridline")
#' #'
#' #' @describeIn get_adlgraphs_global Set a specific global value
#' #'
#' #' @export
#' set_cmapplot_global <- function(value, ..., quietly = FALSE){
#'
#'   # do a get of the specific attribute to make sure it exists.
#'   # this will error if the path is null
#'   p <- get_cmapplot_global(...)
#'
#'   # establish vector of sublocations
#'   names <- unlist(stringr::str_split(c(...), "\\$"))
#'
#'   # get the top-level item
#'   item <- get_cmapplot_global(names[1])
#'
#'   # build a string to evaluate that modifies some element of the item.
#'   str <- paste0(
#'     "item",
#'     ifelse(length(names) > 1, paste0("$", paste(names[-1], collapse = "$")), ""),
#'     " <- ",
#'     ifelse(is.character(value), paste0("'", value, "'"), value)
#'   )
#'
#'   # replace the specific item by evaluating the string
#'   eval(parse(text = str))
#'
#'   # and replace the top level item in the globals env
#'   assign(names[1], item, envir = adlgraphs_global)
#'
#'   # report
#'   if(!quietly){
#'     cat(paste0(
#'       "Item:      ", paste(names, collapse = "$"), "\n",
#'       "Old value: ", ifelse(is.character(p), paste0("'", p, "'"), p), "\n",
#'       "New value: ", ifelse(is.character(value), paste0("'", value, "'"), value)
#'     ))
#'   }
#'   invisible()
#' }
