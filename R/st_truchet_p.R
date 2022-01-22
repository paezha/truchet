st_truchet_p <- function(type = "d", scale_p = 1){

  #' Truchet tiles made with polygons
  #'
  #' @param type A single character to designate a type of tile; currently supported options are "d", "-", "f", "a"
  #' @param scale_p A number to designate the scale of the tile; currently supported options are 1 and 1/2
  #' @return A list with one or more objects of type \code{sf} representing one or more tiles depending on type
  #' @importFrom rlang .data
  #' @export
  #' @examples
  #' st_truchet_p(type = "-")
  #' st_truchet_p(type = "f", scale_p = 1/2)
  #' @note For a discussion of multi-scale Truchet patterns see \url{https://christophercarlson.com/portfolio/multi-scale-truchet-patterns/}

  # Validate inputs
  checkmate::assertChoice(scale_p, c(1, 1/2))
  checkmate::assertChoice(type, c("d", "-", "f", "a"))

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

  # Points
  pts <- data.frame(id = c("points_1", "points_2", "points_1", "points_2"),
                    x = c(0, 0, 1, 1),
                    y = c(0, 1, 1, 0))

  # Convert coordinates to points and then to simple features
  pts <- pts %>%
    sf::st_as_sf(coords = c("x", "y"))

  # Assign constant geometry
  sf::st_agr(pts) <- "constant"

  # Circle segments
  cs <- c(1/3)

  # Create first set of buffers and cast to polygons
  bfs_1 <- pts %>%
    dplyr::mutate(r = cs[1],
                  geometry = pts %>%
                    sf::st_buffer(dist = .data$r) %>%
                    dplyr::pull(.data$geometry)) %>%
    dplyr::select(-.data$r)

  # Assemble base tile
  tile <- data.frame(color = 1,
                     sf::st_geometry(rbind(tile,
                                           bfs_1 %>%
                                             dplyr::select(-.data$id)) %>%
                                       sf::st_union())) %>%
    sf::st_as_sf()

    ## BASE TILE DONE

  # Tile types

  switch(type,

         "d" ={
           ## ADORNMENTS
           pts <- data.frame(id = c("points_1", "points_2", "points_1", "points_2"),
                             x = c(0, 0, 1, 1),
                             y = c(0, 1, 1, 0))

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

           # Intersect lines with tile
           line_1 <- bfs_2 %>%
             dplyr::filter(.data$id == "points_1") %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_2 <- bfs_2 %>%
             dplyr::filter(.data$id == "points_2") %>%
             sf::st_intersection(sf::st_geometry(tile))

           # Buffer the lines
           bfs_2 <- rbind(line_1 %>%
                            sf::st_buffer(dist = 1/6),
                          line_2 %>%
                            sf::st_buffer(dist = 1/6))

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"
           sf::st_agr(bfs_2) <- "constant"

           # Bind BASE TILE with second set of buffers
           tile_1 <- rbind(tile %>%
                             sf::st_difference(bfs_2 %>%
                                                 dplyr::filter(.data$id == "points_1") %>%
                                                 sf::st_union()),
                           bfs_2 %>%
                             dplyr::filter(.data$id == "points_1") %>%
                             dplyr::transmute(color = 2))

           tile_2 <- rbind(tile %>%
                             sf::st_difference(bfs_2 %>%
                                                 dplyr::filter(.data$id == "points_2") %>%
                                                 sf::st_union()),
                           bfs_2 %>%
                             dplyr::filter(.data$id == "points_2") %>%
                             dplyr::transmute(color = 2))
           ## ADORNMENTS DONE

           ## COLLECT TILES
           tiles <- rbind(data.frame(tile_1,
                                     tile = 1),
                          data.frame(tile_2,
                                     tile = 2)) %>%
             sf::st_as_sf()
         },

         "-" = {
           ## ADORNMENTS
           # Points
           pts <- data.frame(id = c("points_1", "points_2", "points_1", "points_2"),
                             x = c(0, 1/2, 1, 1/2),
                             y = c(1/2, 1, 1/2, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Circle segments
           cs <- c(1/6)

           # Create first set of buffers and cast to polygons
           bfs_1 <- pts %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             dplyr::select(-.data$r)


           # Make lines for second set of buffers
           # Make lines
           line_1 <- matrix(c(1/2, 1/2,
                              0, 1),
                            nrow = 2,
                            byrow = FALSE)

           line_2 <- matrix(c(0, 1,
                              1/2, 1/2),
                            nrow = 2,
                            byrow = FALSE)

           # Convert coordinates to lines and then to simple features
           line_1 <- data.frame(id = "line_1",
                                r = 1/6,
                                geometry = sf::st_linestring(line_1) %>%
                                  sf::st_sfc()) %>%
             sf::st_as_sf()

           line_2 <- data.frame(id = "line_2",
                                r = 1/6,
                                geometry = sf::st_linestring(line_2) %>%
                                  sf::st_sfc()) %>%
             sf::st_as_sf()

           # Buffer the lines and join to dots
           bfs_2 <- rbind(line_1 %>%
                            sf::st_buffer(dist = 1/6),
                          line_2 %>%
                            sf::st_buffer(dist = 1/6)) %>%
             dplyr::select(-.data$r)

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"
           sf::st_agr(bfs_1) <- "constant"
           sf::st_agr(bfs_2) <- "constant"

           # Bind BASE TILE with second set of buffers
           tile_1 <- rbind(tile %>%
                             sf::st_difference(bfs_1 %>%
                                                 dplyr::filter(.data$id == "points_1") %>%
                                                 sf::st_union()) %>%
                             sf::st_set_agr("constant") %>%
                             sf::st_difference(bfs_2 %>%
                                                 dplyr::filter(.data$id == "line_1") %>%
                                                 sf::st_union()),
                           bfs_1 %>%
                             dplyr::filter(.data$id == "points_1") %>%
                             dplyr::transmute(color = 2),
                           bfs_2 %>%
                             dplyr::filter(.data$id == "line_1") %>%
                             dplyr::transmute(color = 2))

           tile_2 <- rbind(tile %>%
                             sf::st_difference(bfs_1 %>%
                                                 dplyr::filter(.data$id == "points_2") %>%
                                                 sf::st_union()) %>%
                             sf::st_set_agr("constant") %>%
                             sf::st_difference(bfs_2 %>%
                                                 dplyr::filter(.data$id == "line_2") %>%
                                                 sf::st_union()),
                           bfs_1 %>%
                             dplyr::filter(.data$id == "points_2") %>%
                             dplyr::transmute(color = 2),
                           bfs_2 %>%
                             dplyr::filter(.data$id == "line_2") %>%
                             dplyr::transmute(color = 2))
           ## ADORNMENTS DONE

           ## COLLECT TILES
           tiles <- rbind(data.frame(tile_1,
                                     tile = 1),
                          data.frame(tile_2,
                                     tile = 2)) %>%
             sf::st_as_sf()
         },

         "f" = {
           # THESE TILES ARE AN ABERRRATION, THEY DO NOT FOLLOW THE PROPER RULES, BUT PRODUCE SOMETHING INTERESTING NONETHELESS
           ## ADORNMENTS
           # Points
           pts <- data.frame(id = c("points_1", "points_2", "points_1", "points_2"),
                             x = c(0, 1/2, 1/2, 1),
                             y = c(1/2, 1, 0, 1/2))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Circle segments
           cs <- c(1/6)

           # Create buffers
           bfs_1 <- pts %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             dplyr::select(-.data$r)

           # Points for lines
           pts <- data.frame(id = c("points_1", "points_2"),
                             x = c(1, 0),
                             y = c(1, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Circle segments
           cs <- c(1/2)

           # Create buffers and cast to lines
           bfs_2 <- pts %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Intersect lines with tile
           line_1 <- bfs_2 %>%
             dplyr::filter(.data$id == "points_1") %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_2 <- bfs_2 %>%
             dplyr::filter(.data$id == "points_2") %>%
             sf::st_intersection(sf::st_geometry(tile))

           # Buffer the lines
           bfs_2 <- rbind(line_1 %>%
                            sf::st_buffer(dist = 1/6),
                          line_2 %>%
                            sf::st_buffer(dist = 1/6))


           # Set geometry to constant
           sf::st_agr(tile) <- "constant"
           sf::st_agr(bfs_1) <- "constant"
           sf::st_agr(bfs_2) <- "constant"

           # Bind BASE TILE with second set of buffers
           tile_1 <- rbind(tile %>%
                             sf::st_difference(bfs_1 %>%
                                                 dplyr::filter(.data$id == "points_1") %>%
                                                 sf::st_union()) %>%
                             sf::st_set_agr("constant") %>%
                             sf::st_difference(bfs_2 %>%
                                                 dplyr::filter(.data$id == "points_1") %>%
                                                 sf::st_union()),
                           bfs_1 %>%
                             dplyr::filter(.data$id == "points_1") %>%
                             dplyr::transmute(color = 2),
                           bfs_2 %>%
                             dplyr::filter(.data$id == "points_1") %>%
                             dplyr::transmute(color = 2))

           tile_2 <- rbind(tile %>%
                             sf::st_difference(bfs_1 %>%
                                                 dplyr::filter(.data$id == "points_2") %>%
                                                 sf::st_union()) %>%
                             sf::st_set_agr("constant") %>%
                             sf::st_difference(bfs_2 %>%
                                                 dplyr::filter(.data$id == "points_2") %>%
                                                 sf::st_union()),
                           bfs_1 %>%
                             dplyr::filter(.data$id == "points_2") %>%
                             dplyr::transmute(color = 2),
                           bfs_2 %>%
                             dplyr::filter(.data$id == "points_2") %>%
                             dplyr::transmute(color = 2))

           # Rotate and translate to create alternate tiles
           tile_3 <- tile_1 %>%
             dplyr::mutate(geometry = sf::st_geometry(tile_1) * matrix(c(cos(pi/2),
                                                                         sin(pi/2),
                                                                         -sin(pi/2),
                                                                         cos(pi/2)),
                                                                       nrow = 2,
                                                                       ncol = 2) + c(0, 1))

           tile_4 <- tile_2 %>%
             dplyr::mutate(geometry = sf::st_geometry(tile_2) * matrix(c(cos(pi/2),
                                                                         sin(pi/2),
                                                                         -sin(pi/2),
                                                                         cos(pi/2)),
                                                                       nrow = 2,
                                                                       ncol = 2) + c(0, 1))

           ## ADORNMENTS DONE

           ## COLLECT TILES
           tiles <- rbind(data.frame(tile_1,
                                     tile = 1),
                          data.frame(tile_2,
                                     tile = 2),
                          data.frame(tile_3,
                                     tile = 3),
                          data.frame(tile_4,
                                     tile = 4)) %>%
             sf::st_as_sf()
         },

         "a" = {
           # THESE TILES ARE AN ABERRRATION, THEY DO NOT FOLLOW THE PROPER RULES, BUT PRODUCE SOMETHING INTERESTING NONETHELESS
           ## ADORNMENTS
           # Points
           pts <- data.frame(id = c("points_1", "points_2"),
                             x = c(1, 0),
                             y = c(1, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Circle segments
           cs <- c(1/2)

           # Create buffers and cast to lines
           bfs_2 <- pts %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Intersect lines with tile
           line_1 <- bfs_2 %>%
             dplyr::filter(.data$id == "points_1") %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_2 <- bfs_2 %>%
             dplyr::filter(.data$id == "points_2") %>%
             sf::st_intersection(sf::st_geometry(tile))

           # Buffer the lines
           bfs_2 <- rbind(line_1 %>%
                            sf::st_buffer(dist = 1/6),
                          line_2 %>%
                            sf::st_buffer(dist = 1/6))

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"
           sf::st_agr(bfs_1) <- "constant"
           sf::st_agr(bfs_2) <- "constant"

           # Bind BASE TILE with second set of buffers
           tile_1 <- rbind(tile %>%
                             sf::st_difference(bfs_1 %>%
                                                 dplyr::filter(.data$id == "points_1") %>%
                                                 sf::st_union()) %>%
                             sf::st_set_agr("constant") %>%
                             sf::st_difference(bfs_2 %>%
                                                 dplyr::filter(.data$id == "points_1") %>%
                                                 sf::st_union()),
                           bfs_1 %>%
                             dplyr::filter(.data$id == "points_1") %>%
                             dplyr::transmute(color = 2),
                           bfs_2 %>%
                             dplyr::filter(.data$id == "points_1") %>%
                             dplyr::transmute(color = 2))

           tile_2 <- rbind(tile %>%
                             sf::st_difference(bfs_1 %>%
                                                 dplyr::filter(.data$id == "points_2") %>%
                                                 sf::st_union()) %>%
                             sf::st_set_agr("constant") %>%
                             sf::st_difference(bfs_2 %>%
                                                 dplyr::filter(.data$id == "points_2") %>%
                                                 sf::st_union()),
                           bfs_1 %>%
                             dplyr::filter(.data$id == "points_2") %>%
                             dplyr::transmute(color = 2),
                           bfs_2 %>%
                             dplyr::filter(.data$id == "points_2") %>%
                             dplyr::transmute(color = 2))
           ## ADORNMENTS DONE

           ## COLLECT TILES
           tiles <- rbind(data.frame(tile_1,
                                     tile = 1),
                          data.frame(tile_2,
                                     tile = 2)) %>%
             sf::st_as_sf()
         }
  )

  ## FINISH TILES
  # Translate so that the tiles are centered on the point (0, 0)
  tiles <- tiles %>%
    dplyr::mutate(geometry = sf::st_geometry(tiles) + c(-0.5, -0.5))

  ## TILES DONE

  ## SCALE AS REQUESTED
  if(methods::hasArg(scale_p)){
    tiles <- tiles %>%
      dplyr::mutate(geometry = sf::st_geometry(tiles) * scale_p)
    if(scale_p == 1/2){
      # If scale is 1/2 reverse colors
      tiles <- tiles %>%
        dplyr::mutate(color = dplyr::case_when(color == 1 ~ 2,
                                               color == 2 ~ 1))
    }
  }
  ## FINISH SCALING

  return(tiles)
}
