st_truchet_dissolve <- function(mosaic){

  #' Dissolving the boundaries of individual tiles in Truchet mosaics
  #'
  #' @param mosaic a mosaic produced by function \code{st_truchet_ms}
  #' @return An object of type \code{sf} with the mosaic after dissolving the boundaries of individual tiles
  #' @importFrom rlang .data
  #' @export
  #' @examples
  #' mosaic <- st_truchet_ms()
  #' mosaic <- st_truchet_dissolve(mosaic)

  # Validate inputs

  # Summarize by color to produce individual pieces made of compact segments of mosaic by color
  mosaic_2 <- mosaic %>%
    dplyr::group_by(.data$color) %>%
    dplyr::summarize(color = max(.data$color))

  sf::st_agr(mosaic_2) <- "constant"

  # # Obtain the difference of mosaics of color 1 with respect to 2
  mosaic_3 <- mosaic_2[1,] %>%
    sf::st_difference(mosaic_2[2,]$geometry) %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))

  # # Cast the multipolygon of the opposite color to individual polygons
  mosaic_4 <- mosaic_2[2,] %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))

  # # Bind both colors
  mosaic <- rbind(mosaic_3,
                  mosaic_4)

  return(mosaic)
}
