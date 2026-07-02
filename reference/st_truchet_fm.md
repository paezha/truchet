# Mosaics with flexible Truchet tiles

Mosaics with flexible Truchet tiles

## Usage

``` r
st_truchet_fm(
  df = NULL,
  tiles = c("Al", "Cl"),
  b = 1/2,
  xlim = c(1, 3),
  ylim = c(1, 6)
)
```

## Arguments

- df:

  an (optional) data frame with the following columns: x and y (the
  coordinates of the tiles in a 1 by 1 grid), tiles (characters with
  types of tiles to use for mosaic), b (control of the boundary;
  defaults to 1/2)

- tiles:

  a character vector with types of tiles to use for mosaic (default:
  `c("dr", "dl")`)

- b:

  A number between zero and one that controls the shape of the boundary
  between the two parts of the tile

- xlim:

  a numeric vector of length 2 giving the range of the x coordinates of
  the mosaic (ignored if argument `df` is an input)

- ylim:

  a numeric vector of length 2 giving the range of the y coordinates of
  the mosaic (ignored if argument `df` is an input)

## Value

An object of type `sf` with the tiles arranged as a mosaic

## Note

For a discussion of Truchet patterns see
[http://arearugscarpet.blogspot.com/2014/04/the-curse-of-truchets-tiles.html](http://arearugscarpet.blogspot.com/2014/04/the-curse-of-truchets-tiles.md)

## Examples

``` r
mosaic <- st_truchet_fm()
plot(mosaic)

mosaic <- st_truchet_fm(b = 1/3)
plot(mosaic)
```
