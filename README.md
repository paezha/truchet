
<!-- README.md is generated from README.Rmd. Please edit that file -->

# truchet <a href="https://paezha.github.io/truchet/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->
<!-- badges: end -->

This package implements the multi-scale Truchet tiles of [Christopher
Carlson](https://archive.bridgesmathart.org/2018/bridges2018-39.html) as
explained in this [blog
post](https://christophercarlson.com/portfolio/multi-scale-truchet-patterns/).
This implementation uses the package [{sf}]() to create and manipulate
spatial objects. Two functions are used to create tiles and then to
arrange the tiles in a mosaic. Since the tiles and mosaics are simple
features, they can be plotted using [{ggplot2}]() and `geom_sf()`. In
addition, further manipulations of the tiles (such as buffering) can be
done using the functionality of {sf}.

<!--
Also check http://cambolbro.com/cv/index.html
-->

## Installation

You can install the development version of truchet from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("paezha/truchet")
```

## Example

Once installed, the package can be loaded in the usual way:

``` r
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(purrr)
library(truchet)
```

## Tiles

Currently, these are the tiles that are implemented.

### Tile of type “d” or “\\” and “/” in Carlson’s notation.

Using function `st_truchet_p()` with type “dl” or “dr” produces a simple
features data frame with the elements of a single tile, diagonal left
(“\\”) or diagonal right (“/”). In addition to the geometry of the
spatial elements of the tiles, the data frame includes identifiers for
tiles (i.e., tile 1 is “\\” and tile 2 is “/”) as well as colors. The
function accepts the coordinates of the tile.

``` r
st_truchet_p(x = 1, y = 4, type = "dl") %>%
  ggplot() +
  geom_sf(aes(fill = factor(color)))
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

### Tile of type “-” or “-” and “\|” in Carlson’s notation.

Using function `st_truchet_p()` with type “-” or “\|” produces a simple
features data frame with the elements of a single tile, horizontal (“-”)
or vertical (“\|”). In addition to the geometry of the spatial elements
of the tiles, the data frame includes identifiers for tiles (i.e., tile
1 is “\|” and tile 2 is “-”) as well as colors.

``` r
st_truchet_p(x = 0, y = 0, type = "|") %>%
  ggplot() +
  geom_sf(aes(fill = factor(color)))
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

### Tile of type “f” in Carlson’s notation.

Using function `st_truchet_p()` with type “fnw”, “fne”, “fse”, or “fsw”
produces a data frame with the elements of a single tile. In addition to
the geometry of the spatial elements of the tiles, the data frame
includes identifiers for tiles and colors.

``` r
  ggplot() +
  geom_sf(data = st_truchet_p(x = 0, y = 2, type = "fnw"),
          aes(fill = factor(color))) +
  geom_sf(data = st_truchet_p(x = 2, y = 2, type = "fne"),
          aes(fill = factor(color))) +
  geom_sf(data = st_truchet_p(x = 2, y = 0, type = "fse"),
          aes(fill = factor(color))) +
  geom_sf(data = st_truchet_p(x = 0, y = 0, type = "fsw"),
          aes(fill = factor(color)))
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

## Assembling mosaics

Carlson’s tiles are designed to work at multiple scales. At the moment,
the function to create tiles supports scale 1 (the tiles are squares
with sides of length 1), scale 1/2 (sides of tile are of length 1/2),
and scales 1/4 (sides of tile are of length 1/4). I don’t see much point
doing tiles smaller than this, as their effect can be replicated with
bigger tiles in bigger mosaics, and when used in multi-scale mosaics,
the detail is lost at smaller scales. Argument `scale_p` is used to
select the scale of the output tile (the default is 1). Notice that the
colors alternate at each scale.

``` r
ggplot() +
  geom_sf(data = st_truchet_p(x = 0, y = 0, type = "dl", scale_p = 1),
          aes(fill = factor(color))) +
  geom_sf(data = st_truchet_p(x = 2, y = 0, type = "dl", scale_p = 1/2),
          aes(fill = factor(color))) +
  geom_sf(data = st_truchet_p(x = 4, y = 0, type = "dl", scale_p = 1/4),
          aes(fill = factor(color)))
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

## Assembling a mosaic

It is possible to assemble the tiles into a mosaic manually, but an
additional function offers some functionality to do this task.

Function `st_truchet_ms()` can be used with all default parameters. The
default uses tiles of type `dl` and `dr`, which are positioned at random
(with equal probability) on the mosaic:

``` r
st_truchet_ms() %>%
  ggplot() +
  geom_sf(aes(fill = factor(color)),
          color = NA)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

It is possible to create mosaics with tiles of different sizes; the
parameters `p1`, `p2`, and `p3` control the proportion of tiles at each
scale:

``` r
st_truchet_ms(p1 = 0.6, p2 = 0.3, p3 = 0.1) %>%
  ggplot() +
  geom_sf(aes(fill = factor(color)),
          color = NA)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

Mosaics can use different types of tiles:

``` r
st_truchet_ms(tiles = c("-", "|", "dl")) %>%
  ggplot() +
  geom_sf(aes(fill = factor(color)),
          color = NA)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

## Tile collection

These are all the tiles currently implemented:

``` r
# Extent of mosaic
xlim <- c(0, 9)
ylim <- c(0, 2)

# Create a data frame with the spots for tiles
container <- expand.grid(x = seq(xlim[1], xlim[2], 2),
                         y = c(0, 2)) %>%
  mutate(type = factor(c("dl", "dr", "-", "|", "fnw", "fne", "fsw", "fse", "ane", "asw"),
                       levels = c("dl", "dr", "-", "|", "fnw", "fne", "fsw", "fse", "ane", "asw"),
                       ordered = TRUE))

# Elements for assemblig the mosaic
x_c <- container$x
y_c <- container$y
type <- as.character(container$type)
scale_p <- rep(1, nrow(container))

pmap_dfr(list(x_c, y_c, type, scale_p), st_truchet_p) %>%
  ggplot() + 
  geom_sf(aes(fill = factor(color))) +
  geom_text(data = container,
            aes(x = x,
                y = y,
                label = type),
            nudge_y = 1)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />