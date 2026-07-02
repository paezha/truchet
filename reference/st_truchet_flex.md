# Flexible Truchet tiles

Flexible Truchet tiles

## Usage

``` r
st_truchet_flex(x = 0, y = 0, type = "Al", b = 1/2)
```

## Arguments

- x:

  A number with the x coordinate of the center of the tile

- y:

  A number with the y coordinate of the center of the tile

- type:

  A single character to designate a type of tile; currently supported
  options are "Ac", "Bc", "Cc", "Dc", "As", "Bs", "Cs", "Ds"

- b:

  A number between zero and one that controls the shape of the boundary
  between the two parts of the tile

## Value

A list with one or more objects of type `sf` representing one or more
tiles depending on type

## Note

For a discussion of Truchet patterns see: Robert Bosch & Urchin Colley
(2013) Figurative mosaics from flexible Truchet tiles, Journal of
Mathematics and the Arts, 7:3-4, 122-135,
[10.1080/17513472.2013.838830](https://paezha.github.io/truchet/reference/10.1080/17513472.2013.838830)

## Examples

``` r
st_truchet_flex(type = "Al")
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
#> Simple feature collection with 2 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -0.5 ymin: -0.5 xmax: 0.5 ymax: 0.5
#> CRS:           NA
#>                         geometry color
#> 1 POLYGON ((-0.5 -0.5, -0.5 0...     2
#> 2 POLYGON ((-0.5 0.5, 0.5 0.5...     1
st_truchet_flex(type = "Cl")
#> Simple feature collection with 2 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -0.5 ymin: -0.5 xmax: 0.5 ymax: 0.5
#> CRS:           NA
#>                         geometry color
#> 1 POLYGON ((-0.5 -0.5, -0.5 0...     1
#> 2 POLYGON ((-0.5 0.5, 0.5 0.5...     2
```
