library(hexSticker)
imgurl <-
    system.file("inst/logos/Screenshot_2024-03-02.png",
              package = "MDAW")
sticker(
    imgurl,
    package = "MDAW",
    p_size = 12,
    s_x = 1,
    s_y = .81,
    s_width = .6,
    s_height = .8,
    p_color = "black",
    h_fill = "white",
    h_color = "deepskyblue2",
    filename = "inst/logos/logo.png"
)

