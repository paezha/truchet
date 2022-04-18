st_truchet_boutique <- function(x = 0, y = 0, type = "ribbon_1"){

  #' Flexible Truchet tiles
  #'
  #' @param x A number with the x coordinate of the center of the tile
  #' @param y A number with the y coordinate of the center of the tile
  #' @param type A single character to designate a type of tile; currently supported options are "ribbon_1", "ribbon_2", "ribbon_3", "ribbon_4", "paradise_1", "paradise_2", "paradise_3", "paradise_4", "silk_1", "silk_2", "silk_3", "silk_4", "rainbow_1", "rainbow_2", "cloud_1", "cloud_2", "cloud_3", "cloud_4"
  #' @return A data frame with one or more objects of type \code{sf} representing one or more tiles depending on type
  #' @importFrom rlang .data
  #' @export
  #' @examples
  #' st_truchet_boutique(type = "ribbon_1")
  #' st_truchet_boutique(type = "ribbon_2")
  #' @note For a discussion of variable tiling patterns see: Robert J.Krawczyk (2020) Infinitely Variable Tiling Patterns: From Truchet to Sol LeWitt Revisited, Patterns, 1:5, 1-4, \url{10.1016/j.patter.2020.100084} and
  #' Robert J.Krawczyk (2011) Truchet tilings revisited, Proceedings of ISAMA 2011, 69-77 \url{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.378.5320&rep=rep1&type=pdf#page=69}

  ## NOTE: after creating new tiles add to documentation and update function `st_truchet_ss()` to recognize them

  # Validate inputs
  checkmate::assertChoice(type, c("ribbon_1", "ribbon_2", "ribbon_3", "ribbon_4",
                                  "paradise_1", "paradise_2", "paradise_3", "paradise_4",
                                  "silk_1", "silk_2", "silk_3", "silk_4",
                                  "rainbow_1", "rainbow_2",
                                  "cloud_1", "cloud_2", "cloud_3", "cloud_4"))

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

         "ribbon_1" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 0, 5/12, 1, 1),
                             y = c(0, 7/12, 1, 1, 1, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Circle segments
           cs <- c(1/3)

           # Make lines for first set of buffers
           # Create buffers and cast to lines
           bfs_1 <- pts[c(1, 3, 5, 6),] %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts[c(1, 3, 5, 6),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for second set of buffers
           # Create buffers and cast to lines
           cs <- c(1/2)
           bfs_2 <- pts[c(6),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(6),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for third set of buffers
           # Create buffers and cast to lines
           cs <- c(2/3)
           bfs_3 <- pts[c(6),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(6),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for fourth set of buffers
           # Create buffers and cast to lines
           cs <- c(1/12)
           bfs_4 <- pts[c(2, 4),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(2, 4),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Intersect lines with tile
           line_1 <- bfs_1 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_2 <- bfs_2 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_3 <- bfs_3 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_4 <- bfs_4 %>%
             sf::st_intersection(sf::st_geometry(tile))

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Bind BASE TILE with second set of buffers
           tile <- rbind(line_1,
                         line_2,
                         line_3,
                         line_4)
           ## ADORNMENTS DONE
         },

         "ribbon_2" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 7/12, 1, 1, 1),
                             y = c(0, 1, 1, 1, 7/12, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Circle segments
           cs <- c(1/3)

           # Make lines for first set of buffers
           # Create buffers and cast to lines
           bfs_1 <- pts[c(1, 2, 4, 6),] %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts[c(1, 2, 4, 6),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for second set of buffers
           # Create buffers and cast to lines
           cs <- c(1/2)
           bfs_2 <- pts[c(1),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(1),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for third set of buffers
           # Create buffers and cast to lines
           cs <- c(2/3)
           bfs_3 <- pts[c(1),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(1),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for third set of buffers
           # Create buffers and cast to lines
           cs <- c(1/12)
           bfs_4 <- pts[c(3, 5),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(3, 5),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Intersect lines with tile
           line_1 <- bfs_1 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_2 <- bfs_2 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_3 <- bfs_3 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_4 <- bfs_4 %>%
             sf::st_intersection(sf::st_geometry(tile))

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Bind BASE TILE with second set of buffers
           tile <- rbind(line_1,
                         line_2,
                         line_3,
                         line_4)
           ## ADORNMENTS DONE
         },

         "ribbon_3" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 1, 1, 1, 7/12),
                             y = c(0, 1, 1, 5/12, 0, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Circle segments
           cs <- c(1/3)

           # Make lines for first set of buffers
           # Create buffers and cast to lines
           bfs_1 <- pts[c(1, 2, 3, 5),] %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts[c(1, 2, 3, 5),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for second set of buffers
           # Create buffers and cast to lines
           cs <- c(1/2)
           bfs_2 <- pts[c(2),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(2),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for third set of buffers
           # Create buffers and cast to lines
           cs <- c(2/3)
           bfs_3 <- pts[c(2),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(2),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for third set of buffers
           # Create buffers and cast to lines
           cs <- c(1/12)
           bfs_4 <- pts[c(4, 6),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(4, 6),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Intersect lines with tile
           line_1 <- bfs_1 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_2 <- bfs_2 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_3 <- bfs_3 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_4 <- bfs_4 %>%
             sf::st_intersection(sf::st_geometry(tile))

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Bind BASE TILE with second set of buffers
           tile <- rbind(line_1,
                         line_2,
                         line_3,
                         line_4)
           ## ADORNMENTS DONE
         },

         "ribbon_4" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 0, 1, 1, 7/12),
                             y = c(0, 5/12, 1, 1, 0, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Circle segments
           cs <- c(1/3)

           # Make lines for first set of buffers
           # Create buffers and cast to lines
           bfs_1 <- pts[c(1, 3, 4, 5),] %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts[c(1, 3, 4, 5),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for second set of buffers
           # Create buffers and cast to lines
           cs <- c(1/2)
           bfs_2 <- pts[c(3),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(3),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for third set of buffers
           # Create buffers and cast to lines
           cs <- c(2/3)
           bfs_3 <- pts[c(3),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(3),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for third set of buffers
           # Create buffers and cast to lines
           cs <- c(1/12)
           bfs_4 <- pts[c(2, 6),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(2, 6),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Intersect lines with tile
           line_1 <- bfs_1 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_2 <- bfs_2 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_3 <- bfs_3 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_4 <- bfs_4 %>%
             sf::st_intersection(sf::st_geometry(tile))

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Bind BASE TILE with second set of buffers
           tile <- rbind(line_1,
                         line_2,
                         line_3,
                         line_4)
           ## ADORNMENTS DONE
         },

         "paradise_1" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 1, 1),
                             y = c(0, 1, 1, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Make lines for first set of buffers
           # Circle segments
           cs <- c(1/2, 2/3)

           # Create buffers and cast to lines
           bfs_1 <- rbind(pts[c(1),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(1),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(1),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(1),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           # Make lines for second set of buffers
           # Circle segments
           cs <- c(1/2, 2/3)

           # Create buffers
           bfs_2 <- rbind(pts[c(2),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(2),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(2),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(2),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           bfs_2 <- bfs_2 %>%
             sf::st_difference(sf::st_geometry(bfs_1[1,]))

           # Make lines for third set of buffers
           # Circle segments
           cs <- c(1/2)

           # Create buffers
           bfs_3 <- pts[c(4),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(4),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           bfs_3 <- bfs_3 %>%
             sf::st_difference(sf::st_geometry(bfs_1[1,]))

           # Make lines for fourth set of buffers
           # Create buffers and cast to lines
           # Circle segments
           cs <- c(1/3)

           # Create buffers
           bfs_4 <- pts %>%
             dplyr::mutate(r = cs,
                           geometry = pts %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")


           # Bind and cast to lines

           line_1 <- rbind(bfs_1,
                           bfs_2,
                           bfs_3,
                           bfs_4)  %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING")

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Intersect lines with tile
           tile <- line_1 %>%
             sf::st_intersection(sf::st_geometry(tile))
           ## ADORNMENTS DONE
         },

         "paradise_2" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 1, 1),
                             y = c(0, 1, 1, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Make lines for first set of buffers
           # Circle segments
           cs <- c(1/2, 2/3)

           # Create buffers and cast to lines
           bfs_1 <- rbind(pts[c(2),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(2),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(2),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(2),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           # Make lines for second set of buffers
           # Circle segments
           cs <- c(1/2, 2/3)

           # Create buffers
           bfs_2 <- rbind(pts[c(3),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(3),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(3),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(3),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           bfs_2 <- bfs_2 %>%
             sf::st_difference(sf::st_geometry(bfs_1[1,]))

           # Make lines for third set of buffers
           # Circle segments
           cs <- c(1/2)

           # Create buffers
           bfs_3 <- pts[c(1),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(1),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           bfs_3 <- bfs_3 %>%
             sf::st_difference(sf::st_geometry(bfs_1[1,]))

           # Make lines for fourth set of buffers
           # Create buffers and cast to lines
           # Circle segments
           cs <- c(1/3)

           # Create buffers
           bfs_4 <- pts %>%
             dplyr::mutate(r = cs,
                           geometry = pts %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")


           # Bind and cast to lines

           line_1 <- rbind(bfs_1,
                           bfs_2,
                           bfs_3,
                           bfs_4)  %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING")

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Intersect lines with tile
           tile <- line_1 %>%
             sf::st_intersection(sf::st_geometry(tile))
           ## ADORNMENTS DONE
         },

         "paradise_3" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 1, 1),
                             y = c(0, 1, 1, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Make lines for first set of buffers
           # Circle segments
           cs <- c(1/2, 2/3)

           # Create buffers and cast to lines
           bfs_1 <- rbind(pts[c(3),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(3),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(3),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(3),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           # Make lines for second set of buffers
           # Circle segments
           cs <- c(1/2, 2/3)

           # Create buffers
           bfs_2 <- rbind(pts[c(4),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(4),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(4),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(4),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           bfs_2 <- bfs_2 %>%
             sf::st_difference(sf::st_geometry(bfs_1[1,]))

           # Make lines for third set of buffers
           # Circle segments
           cs <- c(1/2)

           # Create buffers
           bfs_3 <- pts[c(2),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(2),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           bfs_3 <- bfs_3 %>%
             sf::st_difference(sf::st_geometry(bfs_1[1,]))

           # Make lines for fourth set of buffers
           # Create buffers and cast to lines
           # Circle segments
           cs <- c(1/3)

           # Create buffers
           bfs_4 <- pts %>%
             dplyr::mutate(r = cs,
                           geometry = pts %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")


           # Bind and cast to lines

           line_1 <- rbind(bfs_1,
                           bfs_2,
                           bfs_3,
                           bfs_4)  %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING")

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Intersect lines with tile
           tile <- line_1 %>%
             sf::st_intersection(sf::st_geometry(tile))
           ## ADORNMENTS DONE
         },

         "paradise_4" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 1, 1),
                             y = c(0, 1, 1, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Make lines for first set of buffers
           # Circle segments
           cs <- c(1/2, 2/3)

           # Create buffers and cast to lines
           bfs_1 <- rbind(pts[c(4),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(4),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(4),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(4),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           # Make lines for second set of buffers
           # Circle segments
           cs <- c(1/2, 2/3)

           # Create buffers
           bfs_2 <- rbind(pts[c(1),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(1),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(1),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(1),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           bfs_2 <- bfs_2 %>%
             sf::st_difference(sf::st_geometry(bfs_1[1,]))

           # Make lines for third set of buffers
           # Circle segments
           cs <- c(1/2)

           # Create buffers
           bfs_3 <- pts[c(3),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(3),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           bfs_3 <- bfs_3 %>%
             sf::st_difference(sf::st_geometry(bfs_1[1,]))

           # Make lines for fourth set of buffers
           # Create buffers and cast to lines
           # Circle segments
           cs <- c(1/3)

           # Create buffers
           bfs_4 <- pts %>%
             dplyr::mutate(r = cs,
                           geometry = pts %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")


           # Bind and cast to lines

           line_1 <- rbind(bfs_1,
                           bfs_2,
                           bfs_3,
                           bfs_4)  %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING")

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Intersect lines with tile
           tile <- line_1 %>%
             sf::st_intersection(sf::st_geometry(tile))
           ## ADORNMENTS DONE
         },

         "silk_1" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 1, 1, 1, 1/2),
                             y = c(0, 1, 1, 1/2, 0, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Make lines for first set of buffers
           # Circle segments
           cs <- c(1/3)

           # Create buffers and cast to lines
           bfs_1 <- pts[c(1, 2, 3, 5),] %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts[c(1, 2, 3, 5),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           # Make lines for second set of buffers
           # Circle segments
           cs <- c(1/6)

           # Create buffers
           bfs_2 <- pts[c(4, 6),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(4, 6),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           # Make lines for third set of buffers
           # Circle segments
           cs <- c(1/2)

           # Create buffers
           bfs_3 <- pts[c(2, 5),] %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts[c(2, 5),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           bfs_3 <- bfs_3 %>%
             sf::st_difference(sf::st_geometry(bfs_2[1,])) %>%
             sf::st_set_agr("constant") %>%
             sf::st_difference(sf::st_geometry(bfs_2[2,]))

           # Bind and cast to lines
           line_1 <- rbind(bfs_1,
                           bfs_2,
                           bfs_3)  %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING")

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Intersect lines with tile
           tile <- line_1 %>%
             sf::st_intersection(sf::st_geometry(tile))
           ## ADORNMENTS DONE
         },

         "silk_2" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 0, 1, 1, 1/2),
                             y = c(0, 1/2, 1, 1, 0, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Make lines for first set of buffers
           # Circle segments
           cs <- c(1/3)

           # Create buffers and cast to lines
           bfs_1 <- pts[c(1, 3, 3, 5),] %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts[c(1, 3, 4, 5),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           # Make lines for second set of buffers
           # Circle segments
           cs <- c(1/6)

           # Create buffers
           bfs_2 <- pts[c(2, 6),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(2, 6),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           # Make lines for third set of buffers
           # Circle segments
           cs <- c(1/2)

           # Create buffers
           bfs_3 <- pts[c(1, 4),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(1, 4),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           bfs_3 <- bfs_3 %>%
             sf::st_difference(sf::st_geometry(bfs_2[1,])) %>%
             sf::st_set_agr("constant") %>%
             sf::st_difference(sf::st_geometry(bfs_2[2,]))

           # Bind and cast to lines
           line_1 <- rbind(bfs_1,
                           bfs_2,
                           bfs_3)  %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING")

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Intersect lines with tile
           tile <- line_1 %>%
             sf::st_intersection(sf::st_geometry(tile))
           ## ADORNMENTS DONE
         },

         "silk_3" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 0, 1/2, 1, 1),
                             y = c(0, 1/2, 1, 1, 1, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Make lines for first set of buffers
           # Circle segments
           cs <- c(1/3)

           # Create buffers and cast to lines
           bfs_1 <- pts[c(1, 3, 5, 6),] %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts[c(1, 3, 5, 6),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           # Make lines for second set of buffers
           # Circle segments
           cs <- c(1/6)

           # Create buffers
           bfs_2 <- pts[c(2, 4),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(2, 4),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           # Make lines for third set of buffers
           # Circle segments
           cs <- c(1/2)

           # Create buffers
           bfs_3 <- pts[c(3, 6),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(3, 6),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           bfs_3 <- bfs_3 %>%
             sf::st_difference(sf::st_geometry(bfs_2[1,])) %>%
             sf::st_set_agr("constant") %>%
             sf::st_difference(sf::st_geometry(bfs_2[2,]))

           # Bind and cast to lines
           line_1 <- rbind(bfs_1,
                           bfs_2,
                           bfs_3)  %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING")

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Intersect lines with tile
           tile <- line_1 %>%
             sf::st_intersection(sf::st_geometry(tile))
           ## ADORNMENTS DONE
         },

         "silk_4" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 1/2, 1, 1, 1),
                             y = c(0, 1, 1, 1, 1/2, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Make lines for first set of buffers
           # Circle segments
           cs <- c(1/3)

           # Create buffers and cast to lines
           bfs_1 <- pts[c(1, 2, 4, 6),] %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts[c(1, 2, 4, 6),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           # Make lines for second set of buffers
           # Circle segments
           cs <- c(1/6)

           # Create buffers
           bfs_2 <- pts[c(3, 5),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(3, 5),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           # Make lines for third set of buffers
           # Circle segments
           cs <- c(1/2)

           # Create buffers
           bfs_3 <- pts[c(1, 4),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(1, 4),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant")

           bfs_3 <- bfs_3 %>%
             sf::st_difference(sf::st_geometry(bfs_2[1,])) %>%
             sf::st_set_agr("constant") %>%
             sf::st_difference(sf::st_geometry(bfs_2[2,]))

           # Bind and cast to lines
           line_1 <- rbind(bfs_1,
                           bfs_2,
                           bfs_3)  %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING")

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Intersect lines with tile
           tile <- line_1 %>%
             sf::st_intersection(sf::st_geometry(tile))
           ## ADORNMENTS DONE
         },

         "rainbow_1" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 1, 1),
                             y = c(0, 1, 1, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Circle segments
           cs <- c(1/3)

           # Make lines for first set of buffers
           # Create buffers and cast to lines
           bfs_1 <- pts %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for second set of buffers
           # Create buffers and cast to lines
           cs <- c(1/2)
           bfs_2 <- pts[c(1, 3),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(1, 3),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for third set of buffers
           # Create buffers and cast to lines
           cs <- c(2/3)
           bfs_3 <- pts[c(1, 3),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(1, 3),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Intersect lines with tile
           line_1 <- bfs_1 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_2 <- bfs_2 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_3 <- bfs_3 %>%
             sf::st_intersection(sf::st_geometry(tile))

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Bind BASE TILE with second set of buffers
           tile <- rbind(line_1,
                         line_2,
                         line_3)
           ## ADORNMENTS DONE
         },

         "rainbow_2" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 1, 1),
                             y = c(0, 1, 1, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Circle segments
           cs <- c(1/3)

           # Make lines for first set of buffers
           # Create buffers and cast to lines
           bfs_1 <- pts %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for second set of buffers
           # Create buffers and cast to lines
           cs <- c(1/2)
           bfs_2 <- pts[c(2, 4),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(2, 4),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Make lines for third set of buffers
           # Create buffers and cast to lines
           cs <- c(2/3)
           bfs_3 <- pts[c(2, 4),] %>%
             dplyr::mutate(r = cs,
                           geometry = pts[c(2, 4),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry)) %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING") %>%
             dplyr::select(-.data$r)

           # Intersect lines with tile
           line_1 <- bfs_1 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_2 <- bfs_2 %>%
             sf::st_intersection(sf::st_geometry(tile))

           line_3 <- bfs_3 %>%
             sf::st_intersection(sf::st_geometry(tile))

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Bind BASE TILE with second set of buffers
           tile <- rbind(line_1,
                         line_2,
                         line_3)
           ## ADORNMENTS DONE
         },

         "cloud_1" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 1, 1),
                             y = c(0, 1, 1, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Make lines for first set of buffers
           # Circle segments
           cs <- c(1/3, 1/2, 2/3)

           # Create buffers and cast to lines
           bfs_1 <- rbind(pts[c(1),] %>%
                            dplyr::mutate(r = cs[3],
                                          geometry = pts[c(1),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(1),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(1),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(1),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(1),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           # Make lines for second set of buffers
           # Circle segments
           cs <- c(1/3, 1/2, 2/3)

           # Create buffers and cast to lines
           bfs_2 <- rbind(pts[c(2),] %>%
                            dplyr::mutate(r = cs[3],
                                          geometry = pts[c(2),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(2),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(2),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(2),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(2),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           bfs_2 <- bfs_2 %>%
             sf::st_difference(sf::st_geometry(bfs_1[1,]))

           # Make lines for third set of buffers
           # Circle segments
           cs <- c(1/3, 1/2, 2/3)

           # Create buffers and cast to lines
           bfs_3 <- rbind(pts[c(3),] %>%
                            dplyr::mutate(r = cs[3],
                                          geometry = pts[c(3),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(3),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(3),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(3),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(3),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           bfs_3 <- bfs_3 %>%
             sf::st_difference(sf::st_geometry(bfs_2[1,]))

           # Make lines for fourth set of buffers
           # Create buffers and cast to lines
           # Circle segments
           cs <- c(1/3)

           # Create buffers
           bfs_4 <- pts[c(4),] %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts[c(4),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry))

           # Bind and cast to lines
           line_1 <- rbind(bfs_1,
                           bfs_2,
                           bfs_3,
                           bfs_4)  %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING")

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Intersect lines with tile
           tile <- line_1 %>%
             sf::st_intersection(sf::st_geometry(tile))
           ## ADORNMENTS DONE
         },

         "cloud_2" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 1, 1),
                             y = c(0, 1, 1, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Make lines for first set of buffers
           # Circle segments
           cs <- c(1/3, 1/2, 2/3)

           # Create buffers and cast to lines
           bfs_1 <- rbind(pts[c(2),] %>%
                            dplyr::mutate(r = cs[3],
                                          geometry = pts[c(2),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(2),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(2),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(2),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(2),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           # Make lines for second set of buffers
           # Circle segments
           cs <- c(1/3, 1/2, 2/3)

           # Create buffers and cast to lines
           bfs_2 <- rbind(pts[c(3),] %>%
                            dplyr::mutate(r = cs[3],
                                          geometry = pts[c(3),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(3),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(3),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(3),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(3),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           bfs_2 <- bfs_2 %>%
             sf::st_difference(sf::st_geometry(bfs_1[1,]))

           # Make lines for third set of buffers
           # Circle segments
           cs <- c(1/3, 1/2, 2/3)

           # Create buffers and cast to lines
           bfs_3 <- rbind(pts[c(4),] %>%
                            dplyr::mutate(r = cs[3],
                                          geometry = pts[c(4),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(4),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(4),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(4),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(4),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           bfs_3 <- bfs_3 %>%
             sf::st_difference(sf::st_geometry(bfs_2[1,]))

           # Make lines for fourth set of buffers
           # Create buffers and cast to lines
           # Circle segments
           cs <- c(1/3)

           # Create buffers
           bfs_4 <- pts[c(1),] %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts[c(1),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry))

           # Bind and cast to lines
           line_1 <- rbind(bfs_1,
                           bfs_2,
                           bfs_3,
                           bfs_4)  %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING")

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Intersect lines with tile
           tile <- line_1 %>%
             sf::st_intersection(sf::st_geometry(tile))
           ## ADORNMENTS DONE
         },

         "cloud_3" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 1, 1),
                             y = c(0, 1, 1, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Make lines for first set of buffers
           # Circle segments
           cs <- c(1/3, 1/2, 2/3)

           # Create buffers and cast to lines
           bfs_1 <- rbind(pts[c(3),] %>%
                            dplyr::mutate(r = cs[3],
                                          geometry = pts[c(3),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(3),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(3),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(3),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(3),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           # Make lines for second set of buffers
           # Circle segments
           cs <- c(1/3, 1/2, 2/3)

           # Create buffers and cast to lines
           bfs_2 <- rbind(pts[c(4),] %>%
                            dplyr::mutate(r = cs[3],
                                          geometry = pts[c(4),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(4),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(4),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(4),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(4),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           bfs_2 <- bfs_2 %>%
             sf::st_difference(sf::st_geometry(bfs_1[1,]))

           # Make lines for third set of buffers
           # Circle segments
           cs <- c(1/3, 1/2, 2/3)

           # Create buffers and cast to lines
           bfs_3 <- rbind(pts[c(1),] %>%
                            dplyr::mutate(r = cs[3],
                                          geometry = pts[c(1),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(1),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(1),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(1),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(1),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           bfs_3 <- bfs_3 %>%
             sf::st_difference(sf::st_geometry(bfs_2[1,]))

           # Make lines for fourth set of buffers
           # Create buffers and cast to lines
           # Circle segments
           cs <- c(1/3)

           # Create buffers
           bfs_4 <- pts[c(2),] %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts[c(2),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry))

           # Bind and cast to lines
           line_1 <- rbind(bfs_1,
                           bfs_2,
                           bfs_3,
                           bfs_4)  %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING")

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Intersect lines with tile
           tile <- line_1 %>%
             sf::st_intersection(sf::st_geometry(tile))
           ## ADORNMENTS DONE
         },

         "cloud_4" ={
           ## ADORNMENTS
           pts <- data.frame(x = c(0, 0, 1, 1),
                             y = c(0, 1, 1, 0))

           # Convert coordinates to points and then to simple features
           pts <- pts %>%
             sf::st_as_sf(coords = c("x", "y"))

           # Assign constant geometry
           sf::st_agr(pts) <- "constant"

           # Make lines for first set of buffers
           # Circle segments
           cs <- c(1/3, 1/2, 2/3)

           # Create buffers and cast to lines
           bfs_1 <- rbind(pts[c(4),] %>%
                            dplyr::mutate(r = cs[3],
                                          geometry = pts[c(4),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(4),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(4),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(4),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(4),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           # Make lines for second set of buffers
           # Circle segments
           cs <- c(1/3, 1/2, 2/3)

           # Create buffers and cast to lines
           bfs_2 <- rbind(pts[c(1),] %>%
                            dplyr::mutate(r = cs[3],
                                          geometry = pts[c(1),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(1),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(1),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(1),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(1),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           bfs_2 <- bfs_2 %>%
             sf::st_difference(sf::st_geometry(bfs_1[1,]))

           # Make lines for third set of buffers
           # Circle segments
           cs <- c(1/3, 1/2, 2/3)

           # Create buffers and cast to lines
           bfs_3 <- rbind(pts[c(2),] %>%
                            dplyr::mutate(r = cs[3],
                                          geometry = pts[c(2),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(2),] %>%
                            dplyr::mutate(r = cs[2],
                                          geometry = pts[c(2),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry)),
                          pts[c(2),] %>%
                            dplyr::mutate(r = cs[1],
                                          geometry = pts[c(2),] %>%
                                            sf::st_buffer(dist = .data$r) %>%
                                            dplyr::pull(.data$geometry))) %>%
             sf::st_set_agr("constant")

           bfs_3 <- bfs_3 %>%
             sf::st_difference(sf::st_geometry(bfs_2[1,]))

           # Make lines for fourth set of buffers
           # Create buffers and cast to lines
           # Circle segments
           cs <- c(1/3)

           # Create buffers
           bfs_4 <- pts[c(3),] %>%
             dplyr::mutate(r = cs[1],
                           geometry = pts[c(3),] %>%
                             sf::st_buffer(dist = .data$r) %>%
                             dplyr::pull(.data$geometry))

           # Bind and cast to lines
           line_1 <- rbind(bfs_1,
                           bfs_2,
                           bfs_3,
                           bfs_4)  %>%
             sf::st_set_agr("constant") %>%
             sf::st_cast(to = "LINESTRING")

           # Set geometry to constant
           sf::st_agr(tile) <- "constant"

           # Intersect lines with tile
           tile <- line_1 %>%
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
