# Truchet mosaics

Truchet mosaics

## Usage

``` r
st_truchet_ms(
  df = NULL,
  p1 = 1,
  p2 = 0,
  p3 = 0,
  tiles = c("dr", "dl"),
  xlim = c(1, 3),
  ylim = c(1, 6)
)
```

## Arguments

- df:

  an (optional) data frame with the following columns: x and y (the
  coordinates of the tiles in a 1 by 1 grid), tiles (characters with
  types of tiles to use for mosaic), scale_p (the scale of the tile to
  be placed at each coordinate)

- p1:

  a number between 0 and 1 with the proportion of spots in the mosaic to
  cover with tiles of scale 1 (the sum of p1, p2, p3 must be equal to
  one, or less to avoid empty spots in the mosaic)

- p2:

  a number between 0 and 1 with the proportion of spots in the mosaic to
  cover with tiles of scale 1/2

- p3:

  a number between 0 and 1 with the proportion of spots in the mosaic to
  cover with tiles of scale 1/4

- tiles:

  a character vector with types of tiles to use for mosaic (default:
  `c("dr", "dl")`)

- xlim:

  a numeric vector of length 2 giving the range of the x coordinates of
  the mosaic (ignored if argument `df` is an input)

- ylim:

  a numeric vector of length 2 giving the range of the y coordinates of
  the mosaic (ignored if argument `df` is an input)

## Value

An object of type `sf` with the tiles arranged as a mosaic

## Note

For a discussion of multi-scale Truchet patterns see
<https://christophercarlson.com/portfolio/multi-scale-truchet-patterns/>

## Examples

``` r
mosaic <- st_truchet_ms()
plot(mosaic)

mosaic <- st_truchet_ms(p1 = 0.8, p2 = 0.16, p3 = 0.04)
plot(mosaic)

mosaic <- st_truchet_ms(p1 = 0.6, p2 = 0.3, p3 = 0.1, tiles = c("|", "-"))
plot(mosaic)
```
