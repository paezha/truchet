# Truchet tiles made with polygons

Truchet tiles made with polygons

## Usage

``` r
st_truchet_p(x = 0, y = 0, type = "dl", scale_p = 1)
```

## Arguments

- x:

  A number with the x coordinate of the center of the tile

- y:

  A number with the y coordinate of the center of the tile

- type:

  A single character to designate a type of tile; currently supported
  options are "dl", "dr", "-", "\|", "+.", "+", "x.", "tn", "fnw",
  "fne", "fsw", "fse", "ane", "asw"

- scale_p:

  A number to designate the scale of the tile; currently supported
  options are 1, 1/2, and 1/4

## Value

A list with one or more objects of type `sf` representing one or more
tiles depending on type

## Note

For a discussion of multi-scale Truchet patterns see
<https://christophercarlson.com/portfolio/multi-scale-truchet-patterns/>

## Examples

``` r
st_truchet_p(type = "-")
#> Simple feature collection with 4 features and 1 field
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -0.8333333 ymin: -0.8333333 xmax: 0.8333333 ymax: 0.8333333
#> CRS:           NA
#>   color                       geometry
#> 1     1 MULTIPOLYGON (((-0.1739508 ...
#> 2     2 POLYGON ((0.1666667 0.5, 0....
#> 3     2 POLYGON ((0.1666667 -0.5, 0...
#> 4     2 POLYGON ((0.5 0.1666667, 0....
st_truchet_p(type = "fnw", scale_p = 1/2)
#> Simple feature collection with 4 features and 1 field
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -0.4166667 ymin: -0.4166667 xmax: 0.4166667 ymax: 0.4166667
#> CRS:           NA
#>   color                       geometry
#> 1     2 MULTIPOLYGON (((-0.2846519 ...
#> 2     1 POLYGON ((0.25 -0.08333333,...
#> 3     1 POLYGON ((5.551115e-17 -0.3...
#> 4     1 POLYGON ((0.08296216 0.2347...
```
