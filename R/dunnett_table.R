#' Get the frequencies as a GT table
#'
#' This function creates a GT table of the frequencies of a specified variable
#' and has the ability to get the frequencies for one grouping variable.
#' While this can be used on it's own, it was created more to be used in
#' [get_all_freqs()]`, a function that outputs the frequencies of a set of
#' variables to a word doc.
#'
#' @param data An object of type data.frame or tibble. If piping the data into the
#'   function, this is not required.
#' @param x Either a character string or symbol. The variable with which want
#'   to get the frequencies.
#' @param group Either a character string or a symbol. The grouping variable.
#' @param wt Weights. Add if you have a weighting variable and want to get
#'   weighted frequencies
#' @param show_genpop Logical. Should there be a column showing the frequencies
#'   for the general population
#'
#' @export