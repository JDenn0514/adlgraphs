#' This mainly an internal package but can be used externally
#'
#' @keywords internal
#' @export
write_word_table <- function(x, doc) {
  doc %>%
    gto::body_add_gt(value = x) %>%
    officer::body_add_par(value = "") %>%
    officer::body_add_par(value = "")
}




