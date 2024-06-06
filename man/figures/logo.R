library(showtext)
library(hexSticker)

Roboto <- sysfonts::font_add_google("Roboto", "R", regular.wt = 400)


# make the logo
s <- sticker(subplot = "man/figures/adl_logo_copy.png", package = "adlgraphs",
             p_size = 5.25, p_color = "#0A1A50FF", p_family = "Roboto", p_y = 1.05,
             s_x = 1, s_y= 0.3, s_width = 0.3, s_height = 0.3,
             h_fill = "#14A2FCFF", h_color = "white",
             filename = "man/figures/imgfile.png")

sysfonts::font_add("Roboto", "/Users/jacobdennen/Downloads/Roboto,TitilliumWeb/Roboto/Roboto-Bold.ttf")


use_logo("man/figures/imgfile.png")



adl_palettes$categorical
