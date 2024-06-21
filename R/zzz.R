
.onLoad <- function(...) {
  systemfonts::register_variant(name = "Rob Normal", family = "Roboto", weight = "normal")
  systemfonts::register_variant(name = "Rob Heavy", family = "Roboto", weight = "heavy")
  systemfonts::register_variant(name = "Rob Bold", family = "Roboto", weight = "bold")
  systemfonts::register_variant(name = "Rob Medium", family = "Roboto", weight = "medium")

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

