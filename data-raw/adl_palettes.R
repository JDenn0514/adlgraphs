## code to prepare `adl_palettes` dataset goes here


# categorical colors
primary <-  color("#00a0e0")

secondary <- color("#DBDCDD")

tertiary <- color("#69DA78")

pid3 <- color(c("#14A2FC",
                "#60269e",
                "#E84C4C"))




categorical <- color(c("#00a0e0",
                       "#DBDCDD",
                       "#a1d55d",
                       "#0A1A50",
                       "#E84C4C",
                       "#8099FF",
                       "#FFE500",
                       "#2c2e35"))

scales::alpha("#14A2FC", 0.8)


# diverging
likert_4 = color(c("#0A1A50",
                    "#14A2FC",
                    "#DBDCDD",
                    "#595b60"))

likert_6 = color(c("#0A1A50",
                   "#005B98",
                   "#14A2FC",
                   "#FFFFFF",
                   "#DBDCDD",
                   "#595b60",
                   "#2c2e35"))

# sequential
bluescale <- color(c("#0A1A50",
                     "#FFFFFF"))

grayscale <- color(c("#2c2e35",
                     "#FFFFFF"))



adl_palettes <-
  list(
    # categorical
    primary,
    secondary,
    tertiary,
    pid3,
    categorical,
    # divergent
    likert_6,
    likert_4,
    # sequential
    bluescale,
    grayscale
  ) %>%
  rlang::set_names(
    "primary",
    "secondary",
    "tertiary",
    "pid3",
    "categorical",
    "likert_6",
    "likert_4",
    "bluescale",
    "grayscale"
  )

rm(primary, secondary, tertiary, pid3, categorical, likert_4, likert_6, bluescale, grayscale)

usethis::use_data(adl_palettes, overwrite = TRUE)
