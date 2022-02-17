st_truchet_fm <- function(df = NULL, tiles = c("Al", "Cl"), b = 1/2, xlim = c(1, 3), ylim = c(1, 6)){

  #' Mosaics with flexible Truchet tiles
  #'
  #' @param df an (optional) data frame with the following columns: x and y (the coordinates of the tiles in a 1 by 1 grid), tiles (characters with types of tiles to use for mosaic), b (control of the boundary; defaults to 1/2)
  #' @param tiles a character vector with types of tiles to use for mosaic (default: \code{c("dr", "dl")})
  #' @param b A number between zero and one that controls the shape of the boundary between the two parts of the tile
  #' @param xlim a numeric vector of length 2 giving the range of the x coordinates of the mosaic (ignored if argument \code{df} is an input)
  #' @param ylim a numeric vector of length 2 giving the range of the y coordinates of the mosaic (ignored if argument \code{df} is an input)
  #' @return An object of type \code{sf} with the tiles arranged as a mosaic
  #' @importFrom rlang .data
  #' @export
  #' @examples
  #' mosaic <- st_truchet_fm()
  #' plot(mosaic)
  #' mosaic <- st_truchet_fm(b = 1/3)
  #' plot(mosaic)
  #' @note For a discussion of Truchet patterns see \url{http://arearugscarpet.blogspot.com/2014/04/the-curse-of-truchets-tiles.html}


  # Validate inputs
  # Assert xlim
  checkmate::assertAtomicVector(xlim,
                                min.len = 2,
                                max.len = 2)
  # Assert ylim
  checkmate::assertAtomicVector(ylim,
                                min.len = 2,
                                max.len = 2)
  # b must be a value beween zero and 1
  checkmate::assert_number(b, lower = 0, upper = 1)

  # Initialize data frame with coordinates for placing tiles if argument df was not provided
  if(is.null(df)){

    # Create grid for placing tiles using the limits provided xlim and ylim
    df <- data.frame(expand.grid(x = seq(xlim[1], xlim[2], 1),
                                 y = seq(ylim[1], ylim[2], 1))) %>%
      dplyr::mutate(tiles = sample(tiles,
                                   dplyr::n(),
                                   replace = TRUE),
                    b = b)
  }

  # Collect elements for assembling the mosaic
  x_c <- df$x
  y_c <- df$y
  type <- df$tiles
  b <- df$b

  mosaic <- purrr::pmap_dfr(list(x_c, y_c, type, b), st_truchet_flex, .id = "id")

  return(mosaic)
}
