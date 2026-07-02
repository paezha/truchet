# Truchet mosaics

Truchet mosaics

## Usage

``` r
st_truchet_ss(df = NULL, tiles = c("dr", "dl"), xlim = c(1, 3), ylim = c(1, 6))
```

## Arguments

- df:

  an (optional) data frame with the following columns: x and y (the
  coordinates of the tiles in a 1 by 1 grid), tiles (characters with
  types of tiles to use for mosaic), scale_p (the scale of the tile to
  be placed at each coordinate)

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
mosaic <- st_truchet_ss()
plot(mosaic)

mosaic <- st_truchet_ss(tiles = c("dl", "dr"))
plot(mosaic)
```
