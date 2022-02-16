st_truchet_flex <- function(x = 0, y = 0, type = "dl", b = 1/2){

  #' Flexible Truchet tiles
  #'
  #' @param x A number with the x coordinate of the center of the tile
  #' @param y A number with the y coordinate of the center of the tile
  #' @param type A single character to designate a type of tile; currently supported options are "dl", "dr"
  #' @param b A number between zero and one that controls the shape of the boundary between the two parts of the tile
  #' @return A list with one or more objects of type \code{sf} representing one or more tiles depending on type
  #' @importFrom rlang .data
  #' @export
  #' @examples
  #' st_truchet_flex(type = "dl")
  #' st_truchet_flex(type = "dr")
  #' @note For a discussion of Truchet patterns see: Robert Bosch & Urchin Colley (2013) Figurative mosaics from flexible Truchet tiles, Journal of Mathematics and the Arts, 7:3-4, 122-135, \url{10.1080/17513472.2013.838830}

  # Validate inputs
  checkmate::assertChoice(type, c("dl", "dr"))
  # b must be a value beween zero and 1
  checkmate::assert_number(b, lower = 0, upper = 1)

  ## CREATE BASE TILE
  #  Define square polygon
  tile <- matrix(c(0, 0,
                   0, 1,
                   1, 1,
                   1, 0,
                   0, 0),
                 ncol = 2,
                 byrow = TRUE)

  # Convert coordinates to polygons and then to simple features
  tile <- data.frame(geometry = sf::st_polygon(list(tile)) %>%
                       sf::st_sfc()) %>%
    sf::st_as_sf()

  ## BASE TILE DONE

  # Tile types

  switch(type,

         "dl" ={
           ## ADORNMENTS
           t <- seq(0,
                    1,
                    length=50)

           p <- matrix( c(0, 0,
                          b, 1 - b,
                          1, 1),
                        nrow=3,
                        ncol=2,
                        byrow=TRUE
           )

           line_1 <- bezier::bezier(t = t,
                                    p = p)

           line_1 <- data.frame(id = 1,
                                geometry = sf::st_linestring(line_1) %>%
                                  sf::st_geometry()) %>%
             sf::st_sf()

           tile <- tile %>%
             lwgeom::st_split(line_1) %>%
             sf::st_collection_extract() %>%
             dplyr::mutate(color = 2:1)
           ## ADORNMENTS DONE
         },

         "dr" ={
           ## ADORNMENTS
           t <- seq(0,
                    1,
                    length=50)

           p <- matrix( c(0, 1,
                          b, b,
                          1, 0),
                        nrow=3,
                        ncol=2,
                        byrow=TRUE
           )

           line_1 <- bezier::bezier(t = t,
                                    p = p)

           line_1 <- data.frame(id = 1,
                                geometry = sf::st_linestring(line_1) %>%
                                  sf::st_geometry()) %>%
             sf::st_sf()

           tile <- tile %>%
             lwgeom::st_split(line_1) %>%
             sf::st_collection_extract() %>%
             dplyr::mutate(color = 2:1)
           ## ADORNMENTS DONE
         }
  )

  # Translate so that the tiles are centered on the point (0, 0)
  tile <- tile %>%
    dplyr::mutate(geometry = sf::st_geometry(tile) + c(-0.5, - 0.5))

  ## FINISH TILES
  # position at point (x, y)
  tile <- tile %>%
    dplyr::mutate(geometry = sf::st_geometry(tile) + c(x, y))

  ## TILES DONE

  return(tile)
}
