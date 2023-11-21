
.onLoad <- function(...) {
  sysfonts::font_add_google("Titillium Web", "TW", regular.wt = 400)
  sysfonts::font_add_google("Lato", "L", regular.wt = 400)
  showtext::showtext_opts(dpi = 500)
  showtext::showtext_auto(TRUE)

}
