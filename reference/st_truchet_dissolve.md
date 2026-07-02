# Dissolving the boundaries of individual tiles in Truchet mosaics

Dissolving the boundaries of individual tiles in Truchet mosaics

## Usage

``` r
st_truchet_dissolve(mosaic)
```

## Arguments

- mosaic:

  a mosaic produced by function `st_truchet_ms`

## Value

An object of type `sf` with the mosaic after dissolving the boundaries
of individual tiles

## Examples

``` r
mosaic <- st_truchet_ms()
mosaic <- st_truchet_dissolve(mosaic)
```
