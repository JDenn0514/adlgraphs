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
                       "#093c71",
                       "#d8d9da",
                       "#a1d55d",
                       "#60269e",
                       "#db0632",
                       "#eed282",
                       "#2c2e35"))



# diverging
diverge_4 = color(c("#093c71",
                    "#97d0dc",
                    "#d8d9da",
                    "#595b60"))

diverge_6 = color(c("#093c71",
                    "#00a0e0",
                    "#97d0dc",
                    "#d8d9da",
                    "#b0b1b3",
                    "#595b60"))



adl_palettes <- list(primary, secondary, pid3, categorical, diverge_6, diverge_4) %>%
  rlang::set_names("primary", "secondary", "pid3", "categorical", "diverge_6", "diverge_4")




usethis::use_data(adl_palettes, overwrite = TRUE)
