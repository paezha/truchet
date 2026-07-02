# Flexible Truchet tiles

Flexible Truchet tiles

## Usage

``` r
st_truchet_boutique(x = 0, y = 0, type = "ribbon_1")
```

## Arguments

- x:

  A number with the x coordinate of the center of the tile

- y:

  A number with the y coordinate of the center of the tile

- type:

  A single character to designate a type of tile; currently supported
  options are "ribbon_1", "ribbon_2", "ribbon_3", "ribbon_4",
  "paradise_1", "paradise_2", "paradise_3", "paradise_4", "silk_1",
  "silk_2", "silk_3", "silk_4", "rainbow_1", "rainbow_2", "cloud_1",
  "cloud_2", "cloud_3", "cloud_4"

## Value

A data frame with one or more objects of type `sf` representing one or
more tiles depending on type

## Note

For a discussion of variable tiling patterns see: Robert J.Krawczyk
(2020) Infinitely Variable Tiling Patterns: From Truchet to Sol LeWitt
Revisited, Patterns, 1:5, 1-4,
[10.1016/j.patter.2020.100084](https://paezha.github.io/truchet/reference/10.1016/j.patter.2020.100084)
and Robert J.Krawczyk (2011) Truchet tilings revisited, Proceedings of
ISAMA 2011, 69-77
<http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.378.5320&rep=rep1&type=pdf#page=69>

## Examples

``` r
st_truchet_boutique(type = "ribbon_1")
#> Simple feature collection with 8 features and 0 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -0.5 ymin: -0.5 xmax: 0.5 ymax: 0.5
#> CRS:           NA
#>                         geometry
#> 1 LINESTRING (-0.5 -0.1666667...
#> 2 LINESTRING (-0.1666667 0.5,...
#> 3 LINESTRING (0.5 0.1666667, ...
#> 4 LINESTRING (0.1666667 -0.5,...
#> 5 LINESTRING (0 -0.5, 0.00068...
#> 6 LINESTRING (-0.1666667 -0.5...
#> 7 MULTILINESTRING ((-0.416666...
#> 8 LINESTRING (0 0.5, -0.00011...
st_truchet_boutique(type = "ribbon_2")
#> Simple feature collection with 8 features and 0 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -0.5 ymin: -0.5 xmax: 0.5 ymax: 0.5
#> CRS:           NA
#>                         geometry
#> 1 LINESTRING (-0.5 -0.1666667...
#> 2 LINESTRING (-0.1666667 0.5,...
#> 3 LINESTRING (0.5 0.1666667, ...
#> 4 LINESTRING (0.1666667 -0.5,...
#> 5 LINESTRING (-0.5 0, -0.4738...
#> 6 LINESTRING (-0.5 0.1666667,...
#> 7 LINESTRING (0.1666667 0.5, ...
#> 8 LINESTRING (0.5 0, 0.495638...
```
