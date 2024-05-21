## code to prepare `adl_palettes` dataset goes here


# categorical prismatic::prismatic::colors
primary <-  prismatic::color("#14A2FC")

secondary <- prismatic::color("#595b60")

tertiary <- prismatic::color("#69DA78")

pid3 <- prismatic::color(c("#14A2FC",
                "#60269e",
                "#E84C4C"))



categorical <- prismatic::color(c("#14A2FC",
                       "#595b60",
                       "#69DA78",
                       "#0A1A50",
                       "#E84C4C",
                       "#8099FF",
                       "#FFE500",
                       "#FFA828"))



# diverging
likert_4 = prismatic::color(c("#0A1A50",
                   "#14A2FC",
                   "#DBDCDD",
                   "#595b60"))

likert_6 = prismatic::color(c("#0A1A50",
                   "#005B98",
                   "#14A2FC",
                   "#FFFFFF",
                   "#DBDCDD",
                   "#595b60"))

# sequential
bluescale <- prismatic::color(c("#0A1A50",
                     "#E7E8EE"))

grayscale <- prismatic::color(c("#2c2e35FF",
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
