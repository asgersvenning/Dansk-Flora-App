library(tidyverse)
library(magrittr)
library(ggplot2)
library(extrafont)
library(hexSticker)
library(svgparser)
library(grid)
library(terra)
library(showtext)

sticker_mod <- function(subplot, s_x=.8, s_y=.75, s_width=.4, s_height=.5,
                    package, p_x=1, p_y=1.4, p_color="#FFFFFF",
                    p_family="Aller_Rg", p_fontface = "plain", p_size=8,
                    h_size=1.2, h_fill="#1881C2", h_color="#87B13F",
                    spotlight=FALSE, l_x=1, l_y=.5, l_width=3, l_height=3, l_alpha=0.4,
                    url = "",  u_x=1, u_y=0.08, u_color="black",
                    u_family="Aller_Rg", u_size=1.5, u_angle=30,
                    white_around_sticker = FALSE, ...,
                    filename = paste0(package, ".png"), asp=1, dpi = 300) {
  
  hex <- ggplot() +
    geom_hexagon(size = h_size, fill = h_fill, color = NA)
  
  if (inherits(subplot, "character")) {
    d <- data.frame(x=s_x, y=s_y, image=subplot)
    sticker <- hex + geom_image(aes_(x=~x, y=~y, image=~image),
                                d, size=s_width, asp=asp
    )
  } else {
    sticker <- hex + geom_subview(subview=subplot,
                                  x=s_x, y=s_y,
                                  width=s_width, height=s_height
    )
  }
  
  sticker <- sticker +
    geom_hexagon(size = h_size, fill = NA, color = h_color)
  
  if(spotlight)
    sticker <- sticker + geom_subview(subview=spotlight(l_alpha),
                                      x=l_x, y=l_y,
                                      width=l_width, height=l_height
    )
  
  sticker <- sticker + geom_pkgname(package, p_x, p_y,
                                    color = p_color,
                                    family = p_family,
                                    fontface = p_fontface,
                                    size = p_size,
                                    ...)
  
  sticker <- sticker + geom_url(url, x=u_x, y = u_y, color = u_color,
                                family = u_family, size=u_size, angle=u_angle
  )
  
  if (white_around_sticker)
    sticker <- sticker + white_around_hex(size = h_size)
  
  sticker <- sticker + theme_sticker(size = h_size) + coord_cartesian(expand = F, clip = "off")
  
  sticker
}

CMU_serif_path <- extrafont::fonttable() %>% 
  as_tibble %>% 
  filter(FullName == "CMU Serif Roman") %>% 
  pull(fontfile)
  
font_add("CMU Serif",
         CMU_serif_path)
showtext_auto()


icon_dir <- ".svg"

icon <- read_svg(icon_dir, npoints = 100)

ratio <- function(xy) {
  xy[1]/xy[2]
}


hexBase <- ggplot() +
  # geom_text(family="CMU Serif",
  #           size = 32,
  #           color = "#3fae49") +
  annotation_custom(icon,
                    0, 1,
                    0, 0.725) +
  # geom_hexagon(0.5) +
  coord_cartesian(xlim = c(0,1), ylim = c(0, 1), expand = F, clip="off") +
  theme_void() +
  theme(aspect.ratio = 1,
        plot.margin = margin())

hexFinish <- sticker_mod(hexBase, 
        p_family = "CMU Serif",
        p_color = "#cb8a2aff",
        p_size = 155,
        p_y = 1.45,
        s_x = 1,
        s_y = 1,
        s_width = 1.4,
        s_height = 1.4,
        h_size = 5,
        h_fill = "#248f8f",
        h_color = "#cd8b29",
        package="learnDFV")

ggsave("learnDFV_Sticker.png", hexFinish, dpi = 400, width = 4, height = 4.5, scale = 2)
ggsave("readme_files/learnDFV_Sticker.png", dpi = 400, hexFinish, width = 4, height = 4.5, scale = 2)
ggsave("inst/app/www/learnDFV_icon.png", hexFinish, dpi = 400, width = 4, height = 4.5, scale = 2)

