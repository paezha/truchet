st_truchet_ss <- function(df = NULL, tiles = c("dr", "dl"), xlim = c(1, 3), ylim = c(1, 6)){

  #' Truchet mosaics
  #'
  #' @param df an (optional) data frame with the following columns: x and y (the coordinates of the tiles in a 1 by 1 grid), tiles (characters with types of tiles to use for mosaic), scale_p (the scale of the tile to be placed at each coordinate)
  #' @param tiles a character vector with types of tiles to use for mosaic (default: \code{c("dr", "dl")})
  #' @param xlim a numeric vector of length 2 giving the range of the x coordinates of the mosaic (ignored if argument \code{df} is an input)
  #' @param ylim a numeric vector of length 2 giving the range of the y coordinates of the mosaic (ignored if argument \code{df} is an input)
  #' @return An object of type \code{sf} with the tiles arranged as a mosaic
  #' @importFrom rlang .data
  #' @export
  #' @examples
  #' mosaic <- st_truchet_ss()
  #' plot(mosaic)
  #' mosaic <- st_truchet_ss(tiles = c("dl", "dr"))
  #' plot(mosaic)
  #' @note For a discussion of multi-scale Truchet patterns see \url{https://christophercarlson.com/portfolio/multi-scale-truchet-patterns/}

  # Validate inputs

  # Declare types of tiles for boutique function
  # Boutique
  boutique <- c("ribbon_1", "ribbon_2", "ribbon_3", "ribbon_4",
                "paradise_1", "paradise_2", "paradise_3", "paradise_4",
                "silk_1", "silk_2", "silk_3", "silk_4",
                "rainbow_1", "rainbow_2",
                "cloud_1", "cloud_2", "cloud_3", "cloud_4")

  # Linear
  linear <- c("dl", "dr")

  # Assert xlim
  checkmate::assertAtomicVector(xlim,
                                min.len = 2,
                                max.len = 2)
  # Assert ylim
  checkmate::assertAtomicVector(ylim,
                                min.len = 2,
                                max.len = 2)
  # Assert boutique tiles
  checkmate::assert_subset(tiles,
                           c(boutique,
                             linear))


  # Initialize data frame with coordinates for placing tiles if argument df was not provided
  if(is.null(df)){

    # Create grid for placing tiles using the limits provided xlim and ylim
    df <- data.frame(expand.grid(x = seq(xlim[1], xlim[2], 1),
                                 y = seq(ylim[1], ylim[2], 1))) %>%
      dplyr::mutate(tiles = sample(tiles,
                                   dplyr::n(),
                                   replace = TRUE))
  }

  # Collect elements for assembling the mosaic
  x_c <- df$x
  y_c <- df$y
  type <- df$tiles

  ## NOTE: purrr does not like it when .id is used, complains that geometry column not present: why?
  if(checkmate::test_subset(type, boutique)){
    mosaic <- purrr::pmap_dfr(list(x_c, y_c, type), st_truchet_boutique)
    return(mosaic)
  }else
    if(checkmate::test_subset(type, linear)){
    mosaic <- purrr::pmap_dfr(list(x_c, y_c, type), st_truchet_l)
    return(mosaic)
    }else{
      warning("Mixing boutique and linear tiles not supported: no output produced")
  }
}
