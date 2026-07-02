# Truchet tiles made with polygons

Truchet tiles made with polygons

## Usage

``` r
st_truchet_l(x = 0, y = 0, type = "dl")
```

## Arguments

- x:

  A number with the x coordinate of the center of the tile

- y:

  A number with the y coordinate of the center of the tile

- type:

  A single character to designate a type of tile; currently supported
  options are "dl", "dr"

## Value

A list with one or more objects of type `sf` representing one or more
tiles depending on type

## Note

For a discussion of Truchet patterns see
[http://arearugscarpet.blogspot.com/2014/04/the-curse-of-truchets-tiles.html](http://arearugscarpet.blogspot.com/2014/04/the-curse-of-truchets-tiles.md)

## Examples

``` r
st_truchet_l(type = "dl")
#> Simple feature collection with 2 features and 0 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -0.5 ymin: -0.5 xmax: 0.5 ymax: 0.5
#> CRS:           NA
#>                         geometry
#> 1 LINESTRING (-0.5 0, -0.4738...
#> 2 LINESTRING (0.5 0, 0.473832...
st_truchet_l(type = "dr")
#> Simple feature collection with 2 features and 0 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -0.5 ymin: -0.5 xmax: 0.5 ymax: 0.5
#> CRS:           NA
#>                         geometry
#> 1 LINESTRING (0 0.5, -0.00068...
#> 2 LINESTRING (0 -0.5, 0.00068...
```
