## code to prepare `adl_palettes` dataset goes here


# categorical colors
primary <-  color("#00a0e0")

secondary <- color("#b0b1b3")

tertiary <- color("#a1d55d")

pid3 <- color(c("#00a0e0",
                "#60269e",
                "#db0632"))


categorical <- color(c("#00a0e0",
                       "#b0b1b3",
                       "#a1d55d",
                       "#093c71",
                       "#db0632",
                       "#60269e",
                       "#eed282",
                       "#2c2e35"))



# diverging
likert_4 = color(c("#093c71",
                    "#97d0dc",
                    "#d8d9da",
                    "#595b60"))

likert_6 = color(c("#093c71",
                    "#00a0e0",
                    "#97d0dc",
                    "#d8d9da",
                    "#b0b1b3",
                    "#595b60"))

# sequential
bluescale <- color(c("#093c71",
                     "#97d0dc"))

grayscale <- color(c("#2c2e35",
                     "#d8d9da"))



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
