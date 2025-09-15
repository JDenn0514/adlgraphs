## code to prepare `adl_palettes` dataset goes here


# categorical prismatic::prismatic::colors
primary <-  prismatic::color("#14A2FC")

secondary <- prismatic::color("#B0B1B3")

tertiary <- prismatic::color("#E84C4C")

pid3 <- prismatic::color(c(
  "#14A2FC",
  "#60269e",
  "#E84C4C"
))

binary <- prismatic::color(c(
  "#14A2FC",
  "#B0B1B3"
))


categorical <- prismatic::color(c(
  "#14A2FC",
  "#B0B1B3",
  "#E84C4C",
  "#0A1A50",
  "#FFE500",
  "#69DA78",
  "#60269e",
  "#FFA828"
))




# diverging
likert_4 = prismatic::color(c(
  "#0A1A50",
  "#14A2FC",
  "#DBDCDD",
  "#595b60"
))

likert_6 = prismatic::color(c("#0A1A50",
                   "#005B98",
                   "#14A2FC",
                   "#DBDCDD",
                   "#B0B1B3",
                   "#595b60"))

# sequential
bluescale <- prismatic::color(c(
  "#0A1A50",
  "#E7E8EE"
))





adl_palettes <-
  list(
    # categorical
    primary,
    secondary,
    tertiary,
    binary,
    pid3,
    categorical,
    # divergent
    likert_6,
    likert_4,
    # sequential
    bluescale
  ) %>%
  stats::setNames(
    "primary",
    "secondary",
    "tertiary",
    "binary",
    "pid3",
    "categorical",
    "likert_6",
    "likert_4",
    "bluescale"
  )


usethis::use_data(adl_palettes, overwrite = TRUE)
