make_basic_df <- function() {
  tibble::tibble(
    id = 1:8,
    grp = rep(c("A", "B"), each = 4),
    x1 = c("yes", "no", "yes", NA, "no", "no", "yes", "no"),
    x2 = c("yes", "yes", "no", "no", "yes", "no", "no", NA),
    wts = c(1, 2, 1, 1, 1, 3, 1, 2)
  )
}

# Add simple variable labels and value labels where applicable
label_vars <- function(df) {
  attr(df$x1, "label") <- "Q1. Agree?"
  attr(df$x2, "label") <- "Q2. Color"
  # Provide value labels, but they’re illustrative only
  attr(df$x1, "labels") <- c(yes = "yes", no = "no")
  attr(df$x2, "labels") <- c(yes = "yes", no = "no")
  df
}

# Small survey design
make_svy <- function(df) {
  survey::svydesign(ids = ~1, weights = ~wts, data = df)
}
