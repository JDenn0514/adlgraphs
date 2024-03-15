
.onLoad <- function(...) {
  sysfonts::font_add_google("Titillium Web", "TW", regular.wt = 400)
  sysfonts::font_add_google("Lato", "L", regular.wt = 400)
  showtext::showtext_opts(dpi = 500)
  showtext::showtext_auto(TRUE)
}

utils::globalVariables(
  c(
    "estimate", "wts", ".", "adl_palettes", "lower", "upper", "x", "correlation",
    "trad_avg_n", "x_title", "estimate", "std.error", "var_class",
    "contrasts_type", "conf.high", "conf.low", "ss", "p.value", "label", "x_f",
    ".data", "n", "pct", "General Population", "pct_lab", "group_f", "sd",
    "value_label"
  )
)
