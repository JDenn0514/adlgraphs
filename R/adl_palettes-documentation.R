#' Complete list of available adl official color palettes
#'
#' A list of all color palettes to choose from, and preview directly in the console.
#'
#' Pretty print and plot methods are powered by the \href{https://CRAN.R-project.org/package=prismatic}{prismatic}
#' package. Each palette is stored as a `color` object to enact this behaviour.
#'
#'
#' @format A list of 9 \code{color} objects elements.
#' \describe{
#'   \item{primary}{Primary categorical color}
#'   \item{secondary}{Seconday categorical color}
#'   \item{tertiary}{Tertiary categorical color}
#'   \item{pid3}{Palette for partisanship/ideology (categorical)}
#'   \item{categorical}{Palette for categorical data for up to 7 groups (categorical)}
#'   \item{likert_6}{Palette for likert scales with 6 response options (diverging)}
#'   \item{likert_4}{Palette for likert scales with 4 response options (diverging)}
#'   \item{bluescale}{Blue scale color palette (sequential)}
#'   \item{grayscale}{Grayscale color palette (sequential)}
#' }
#'
#' @examples
#' adl_palettes$primary
#' adl_palettes$secondary
#' adl_palettes$tertiary
#' adl_palettes$pid3
#' adl_palettes$categorical
#' adl_palettes$likert_6
#' adl_palettes$likert_4
#' adl_palettes$bluescale
#' adl_palettes$grayscale
#'
"adl_palettes"



