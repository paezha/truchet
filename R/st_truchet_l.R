st_truchet_l <- function(x = 0, y = 0, type = "dl"){

  #' Truchet tiles made with polygons
  #'
  #' @param x A number with the x coordinate of the center of the tile
  #' @param y A number with the y coordinate of the center of the tile
  #' @param type A single character to designate a type of tile; currently supported options are "dl", "dr"
  #' @return A list with one or more objects of type \code{sf} representing one or more tiles depending on type
  #' @importFrom rlang .data
  #' @export
  #' @examples
  #' st_truchet_l(type = "dl")
  #' st_truchet_l(type = "dr")
  #' @note For a discussion of Truchet patterns see \url{http://arearugscarpet.blogspot.com/2014/04/the-curse-of-truchets-tiles.html}

  # Validate inputs

  checkmate::assertChoice(type, c("dl", "dr"))

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
           pts <- data.frame(x = c(0, 1),
                             y = c(0, 1))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Circle segments
           cs <- c(1/2)

           # Make lines for second set of buffers
           # Create buffers and cast to lines
           bfs_2 <- pts %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Intersect lines with tile to give tile motiff
           tile <- bfs_2 %>%
             sf::st_intersection(sf::st_geometry(tile))
           ## ADORNMENTS DONE
         },

         "dr" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 1),
                             y = c(1, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Circle segments
           cs <- c(1/2)

           # Make lines for second set of buffers
           # Create buffers and cast to lines
           bfs_2 <- pts %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Intersect lines with tile to give tile motiff
           tile <- bfs_2 %>%
             sf::st_intersection(sf::st_geometry(tile))
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
