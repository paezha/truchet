st_truchet_ms <- function(t1, t2 = NULL, prop = 0.5, xlim = c(1, 8), ylim = c(1, 12), df = NULL){

  #' Truchet mosaics
  #'
  #' @param t1 a \code{sf} data frame with truchet tile(s) produced by \link{st_truchet_p} at scale 1; these tiles are squares with sides of length 1
  #' @param t2 an (optional) \code{sf} data frame with truchet tile(s) produced by \link{st_truchet_p} at scale 1/2; these tiles are squares with sides of length 1/2
  #' @param prop a number between 0 and 1 with the proportion of spots in the mosaic to cover with tiles of scale 1
  #' @param xlim a numeric vector of length 2 giving the range of the x coordinates of the mosaic (ignored if argument \code{df} is an input)
  #' @param ylim a numeric vector of length 2 giving the range of the y coordinates of the mosaic (ignored if argument \code{df} is an input)
  #' @param df a data frame with the following columns: x and y (the coordinates of the tiles in a 1 by 1 grid) and scale_p (the scale of the tile to be placed at each coordinate)
  #' @return A list with two objects of type \code{sf} representing the container for the mosaic and the tiles arranged as a mosaic
  #' @importFrom rlang .data
  #' @export
  #' @examples
  #' tiles_1 <- st_truchet_p(type = "-", scale_p = 1)
  #' tiles_2 <- st_truchet_p(type = "f", scale_p = 1/2)
  #' mosaic <- st_truchet_ms(tiles_1, tiles_2)
  #' @note For a discussion of multi-scale Truchet patterns see \url{https://christophercarlson.com/portfolio/multi-scale-truchet-patterns/}

  # Validate inputs
  checkmate::assertNumber(prop,
                          lower = 0,
                          upper = 1)
  checkmate::assertAtomicVector(xlim,
                                min.len = 2,
                                max.len = 2)
  checkmate::assertAtomicVector(ylim,
                                min.len = 2,
                                max.len = 2)

  # Set the proportion of tiles at scale 1 to 1 if tiles at scale 1/2 not provided
  if(is.null(t2)){
    prop = 1
  }

  # Find the number of different tiles at each scale
  n_1 <- unique(t1$tile)
  if(length(n_1) == 1){
    n_1 <- c(n_1, n_1)
  }
  if(!is.null(t2)){
    n_2 <- unique(t2$tile)
    if(length(n_2) == 1){
      n_1 <- c(n_2, n_2)
    }
  }

  # Initialize data frame with coordinates for placing tiles if argument df was not provided
  if(is.null(df)){
    # Flag to indicate that the data frame is generated internally
    df_internal <- TRUE

    # Create grid for placing tiles using the limits provided xlim and ylim
    df <- data.frame(expand.grid(x = seq(xlim[1], xlim[2], 1),
                                 y = seq(ylim[1], ylim[2], 1)))
  }else{
    df_internal <- FALSE
  }

  # Initialize mosaic
  mosaic <- data.frame()

  if(df_internal){
    for(i in 1:nrow(df)){
      # Randomly select the scale of the mosaic
      if(stats::rbinom(1, 1, p = prop) == 1){
        mosaic <- rbind(mosaic,
                        t1 %>%
                          # Randomly sample from available tiles at this scale
                          dplyr::filter(.data$tile == sample(n_1, 1)) %>%
                          dplyr::mutate(group = i,
                                        geometry = .data$geometry + c(df[i, 1], df[i, 2])) %>%
                          sf::st_as_sf())

      }else{
        mosaic <- rbind(mosaic,
                        t2 %>%
                          # Randomly sample from available tiles at this scale
                          dplyr::filter(.data$tile == sample(n_2, 1)) %>%
                          dplyr::mutate(group = i,
                                        geometry = .data$geometry + c(df[i, 1] - 0.25, df[i, 2] - 0.25)) %>%
                          sf::st_as_sf(),
                        t2 %>%
                          # Randomly sample from available tiles at this scale
                          dplyr::filter(.data$tile == sample(2, 1)) %>%
                          dplyr::mutate(group = i,
                                        geometry = .data$geometry + c(df[i, 1] - 0.25, df[i, 2] + 0.25)) %>%
                          sf::st_as_sf(),
                        t2 %>%
                          # Randomly sample from available tiles at this scale
                          dplyr::filter(.data$tile == sample(2, 1)) %>%
                          dplyr::mutate(group = i,
                                        geometry = .data$geometry + c(df[i, 1] + 0.25, df[i, 2] - 0.25)) %>%
                          sf::st_as_sf(),
                        t2 %>%
                          # Randomly sample from available tiles at this scale
                          dplyr::filter(.data$tile == sample(2, 1)) %>%
                          dplyr::mutate(group = i,
                                        geometry = .data$geometry + c(df[i, 1] + 0.25, df[i, 2] + 0.25)) %>%
                          sf::st_as_sf())
      }}}else{
        for(i in 1:nrow(df)){
          # Assign tile by desired scale
          if(df$scale_p[i] == 1){
            mosaic <- rbind(mosaic,
                            t1 %>%
                              # Randomly sample from available tiles at this scale
                              dplyr::filter(.data$tile == sample(n_1, 1)) %>%
                              dplyr::mutate(group = i,
                                            geometry = .data$geometry + c(df[i, 1], df[i, 2])) %>%
                              sf::st_as_sf())

          }else{
            mosaic <- rbind(mosaic,
                            t2 %>%
                              # Randomly sample from available tiles at this scale
                              dplyr::filter(.data$tile == sample(n_2, 1)) %>%
                              dplyr::mutate(group = i,
                                            geometry = .data$geometry + c(df[i, 1] - 0.25, df[i, 2] - 0.25)) %>%
                              sf::st_as_sf(),
                            t2 %>%
                              # Randomly sample from available tiles at this scale
                              dplyr::filter(.data$tile == sample(2, 1)) %>%
                              dplyr::mutate(group = i,
                                            geometry = .data$geometry + c(df[i, 1] - 0.25, df[i, 2] + 0.25)) %>%
                              sf::st_as_sf(),
                            t2 %>%
                              # Randomly sample from available tiles at this scale
                              dplyr::filter(.data$tile == sample(2, 1)) %>%
                              dplyr::mutate(group = i,
                                            geometry = .data$geometry + c(df[i, 1] + 0.25, df[i, 2] - 0.25)) %>%
                              sf::st_as_sf(),
                            t2 %>%
                              # Randomly sample from available tiles at this scale
                              dplyr::filter(.data$tile == sample(2, 1)) %>%
                              dplyr::mutate(group = i,
                                            geometry = .data$geometry + c(df[i, 1] + 0.25, df[i, 2] + 0.25)) %>%
                              sf::st_as_sf())
          }
        }
      }

  # Summarize by color to give individual pieces made of compact segments of mosaic by color
  mosaic_2 <- mosaic %>%
    dplyr::group_by(.data$color) %>%
    dplyr::summarize(color = max(.data$color))

  sf::st_agr(mosaic_2) <- "constant"

  # Obtain the difference of mosaics of color 1 with respect to 2
  mosaic_3 <- mosaic_2[1,] %>%
    sf::st_difference(mosaic_2[2,]$geometry) %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))

  # Cast the multipolygon of the opposite color to individual polygons
  mosaic_4 <- mosaic_2[2,] %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))

  # Bind both colors
  mosaic <- rbind(mosaic_3,
                  mosaic_4)

  return(mosaic)
}
