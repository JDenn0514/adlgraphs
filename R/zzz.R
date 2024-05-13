
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
    "value_label", "race_1", "race_2", "race_3", "race_4", "race_5", "race_6",
    "hispanic", "white_b", "black_b", "aapi_b", "native_b", "other_b", "race_f",
    "sex", "edu", "income", "age", "ideology", "partyid8", "partyid7",
    "progressive", "religpew", "religpew_f", "religpew_evan_f", "born_again",
    "jewish", "reltrad_f"
  )
)
