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




