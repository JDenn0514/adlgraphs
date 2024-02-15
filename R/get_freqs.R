# # Calculate simple frequencies
#
# get_freqs <- function(
#     df,
#     x,
#     wt = NULL,
#     cum = FALSE,
#     drop_na = FALSE,
#     to_factor = FALSE
# ) {
#   # Because this is going to be used in get_freq_table() and thus in
#   # get_all_freqs() it needs to accept both strings and symbols. As such we need
#   # to use rlang::enexpr() to capture the expressions supplied in "x".
#   # rlang::enexpr() returns a naked expression of the argument supplied in "x".
#   # This is what allows the input to be either a string or a symbol.
#   x <- rlang::enexpr(x)
#   # if the object supplied in "x" is not a character...
#   if (!is.character(x)) {
#     # capture x and convert to a symbol object with ensym()
#     #then use as_name() to make it a string
#     x <- rlang::as_name(rlang::ensym(x))
#   }
#
#   if (is.null(wt)) {
#
#     if (drop_na == FALSE) {
#
#       if (to_factor == FALSE) {
#
#         df %>%
#           # drop NAs from x, use tidyselect::all_of() because we using an
#           # external vector
#           tidyr::drop_na(tidyselect::all_of(x)) %>%
#           # get the frequency of the data
#           dplyr::count(.data[[x]]) %>%
#           dplyr::mutate(
#             # get the percentages
#             pct = prop.table(n),
#             # round the percentage
#             pct = round(pct, 3),
#             pct_lab = scales::percent(pct, accuracy = 0.1)
#           )
#       } else {
#         df %>%
#           # drop NAs from x
#           tidyr::drop_na({{ x }}) %>%
#           dplyr::mutate(x_f := haven::as_factor(.data[[x]])) %>%
#           dplyr::count(x_f) %>%
#           dplyr::mutate(
#             pct = prop.table(n),
#             pct = round(pct, 3),
#             pct = scales::percent(pct, accuracy = 0.1)
#           )
#
#       }
#
#     }
#
#   }
#
# }
#
#
# get_freqs(pol_pos, trad_n)
#
#
