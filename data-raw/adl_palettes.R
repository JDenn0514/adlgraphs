## code to prepare `DATASET` dataset goes here
library(rlang)
library(prismatic)

# categorical colors
primary <-  color("#00a0e0")

secondary <- color("#a1d55d")

tertiary <- color("#093c71")

pid3 <- color(c("#00a0e0",
                "#60269e",
                "#db0632"))


categorical <- color(c("#00a0e0",
                       "#b0b1b3",
                       "#093c71",
                       "#a1d55d",
                       "#60269e",
                       "#db0632",
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




usethis::use_data(adl_palettes, overwrite = TRUE)
