## code to prepare `adl_palettes` dataset goes here


# categorical colors
primary <-  color("#14A2FC")

secondary <- color("#DBDCDD")

tertiary <- color("#69DA78")

pid3 <- color(c("#14A2FC",
                "#60269e",
                "#E84C4C"))



categorical <- color(c("#14A2FC",
                       "#DBDCDD",
                       "#69DA78",
                       "#0A1A50",
                       "#E84C4C",
                       "#8099FF",
                       "#FFE500",
                       "#FFA828"))



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
                   "#595b60"))

# sequential
bluescale <- color(c("#0A1A50",
                     "#E7E8EE"))

grayscale <- color(c("#2c2e35FF",
                     "#e1e3e4"))



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
