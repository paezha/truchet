## code to create hex sticker

library(ggplot2)
library(here)
library(hexbin)
library(hexSticker)
library(sf)
library(showtext)
library(truchet)

font_add_google("Pacifico")

# Create tiles for hex sticker
set.seed(01222022)
tiles_1 <- st_truchet_p(type = "f")
mosaic <- st_truchet_ms(tiles_1 %>% dplyr::filter(tile == 2))
container <- mosaic[["container"]]
mosaic <- mosaic[["mosaic"]]

# Now create a hexagon for the sticker:
# Length of a side of the polygon
side_l <- 1.5
coords_hex <- hexcoords(dx = 2 * sqrt(side_l^2 - (side_l/2)^2),
                        dy = side_l)


corners <- matrix(c(coords_hex$x + 4,
                    coords_hex$x[1] + 4,
                    coords_hex$y + 6,
                    coords_hex$y[1] + 6),
                  ncol = 2,
                  byrow = FALSE)

# Turn hexagon into sf
hexagon <- data.frame(geometry = sf::st_polygon(list(corners)) %>%
                     sf::st_sfc()) %>%
  sf::st_as_sf() %>%
  st_set_agr("constant")

# Crop mosaic to hexagon
mosaic <- mosaic %>%
  st_set_agr("constant") %>%
  st_intersection(hexagon)

# Plot
hp <- ggplot() +
  geom_sf(data = mosaic,
          aes(fill = factor(color), alpha = 0.5),
          color = NA,
          size = 1) +
  theme_void() +
  scale_fill_manual(values = c("2" = "white", "1" = MexBrewer::mex.brewer("Alacena")[9])) +
  theme(legend.position = "none")

hs <- sticker(hp,
              s_x=1,
              s_y=1,
              s_width = 2.2,
              s_height = 2.2,
              package="truchet",
              h_fill="white",
              h_color = MexBrewer::mex.brewer("Alacena")[9],
              p_size = 11,
              p_x = 1,
              p_y = 1.1,
              p_family = "Pacifico",
              p_fontface = "bold",
              p_color = "black",
              h_size = 2.3,
              url = "paezha.github.io/truchet",
              u_color = "white",
              u_family = "Roboto Condensed",
              u_x = 0.27,
              u_y = 1.58,
              u_size = 1.5,
              filename = paste0(here(), "/man/figures/truchet.png"))
