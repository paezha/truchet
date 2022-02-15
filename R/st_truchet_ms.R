st_truchet_ms <- function(df = NULL, p1 = 1, p2 = 0, p3 = 0, tiles = c("dr", "dl"), xlim = c(1, 3), ylim = c(1, 6)){

  #' Truchet mosaics
  #'
  #' @param df an (optional) data frame with the following columns: x and y (the coordinates of the tiles in a 1 by 1 grid), tiles (characters with types of tiles to use for mosaic), scale_p (the scale of the tile to be placed at each coordinate)
  #' @param p1 a number between 0 and 1 with the proportion of spots in the mosaic to cover with tiles of scale 1 (the sum of p1, p2, p3 must be equal to one, or less to avoid empty spots in the mosaic)
  #' @param p2 a number between 0 and 1 with the proportion of spots in the mosaic to cover with tiles of scale 1/2
  #' @param p3 a number between 0 and 1 with the proportion of spots in the mosaic to cover with tiles of scale 1/4
  #' @param tiles a character vector with types of tiles to use for mosaic (default: \code{c("dr", "dl")})
  #' @param xlim a numeric vector of length 2 giving the range of the x coordinates of the mosaic (ignored if argument \code{df} is an input)
  #' @param ylim a numeric vector of length 2 giving the range of the y coordinates of the mosaic (ignored if argument \code{df} is an input)
  #' @return An object of type \code{sf} with the tiles arranged as a mosaic
  #' @importFrom rlang .data
  #' @export
  #' @examples
  #' mosaic <- st_truchet_ms()
  #' mosaic <- st_truchet_ms(p1 = 0.8, p2 = 0.16, p3 = 0.04)
  #' mosaic <- st_truchet_ms(p1 = 0.6, p2 = 0.3, p3 = 0.1, tiles = c("|", "-"))
  #' @note For a discussion of multi-scale Truchet patterns see \url{https://christophercarlson.com/portfolio/multi-scale-truchet-patterns/}

  # Validate inputs
  # Assert proportions
  checkmate::assertNumber(p1,
                          lower = 0,
                          upper = 1)
  checkmate::assertNumber(p2,
                          lower = 0,
                          upper = 1)
  checkmate::assertNumber(p3,
                          lower = 0,
                          upper = 1)
  # Assert sum of proportions
  checkmate::assertTRUE(p1 + p2 + p3 <= 1)
  # Assert xlim
  checkmate::assertAtomicVector(xlim,
                                min.len = 2,
                                max.len = 2)
  # Assert ylim
  checkmate::assertAtomicVector(ylim,
                                min.len = 2,
                                max.len = 2)

  # Initialize data frame with coordinates for placing tiles if argument df was not provided
  if(is.null(df)){

    # Create grid for placing tiles using the limits provided xlim and ylim
    df <- data.frame(expand.grid(x = seq(xlim[1], xlim[2], 1),
                                 y = seq(ylim[1], ylim[2], 1)))

    # Adjust container to accommodate tiles at multiple scales:

    df_1 <- df %>%
      dplyr::slice_sample(prop = p1)

    df_2 <- df %>%
      dplyr::anti_join(df_1,
                       by = c("x", "y"))

    df_2 <- df_2 %>%
      dplyr::slice_sample(prop = ifelse(p2 != 0 | p3 != 0,
                                        p2/(p2 + p3),
                                        0))

    df_3 <- df %>%
      dplyr::anti_join(df_1 %>%
                         rbind(df_2),
                       by = c("x", "y"))

    df <- rbind(df_1,
                df_2,
                df_3) %>%
      dplyr::mutate(tiles = sample(tiles,
                                   dplyr::n(),
                                   replace = TRUE),
                    scale_p = c(rep(1,
                                    nrow(df_1)),
                                rep(1/2,
                                    nrow(df_2)),
                                rep(1/4,
                                    nrow(df_3))))
  }

  # Adjust points for multiscale mosaics
  # Scale 1
  df_1 <- df %>%
    dplyr::filter(scale_p == 1)

  # Scale 2
  df_2 <- df %>%
    dplyr::filter(scale_p == 1/2) %>%
    dplyr::mutate(x_1 = -0.25, y_1 = 0.25,
                  x_2 = 0.25, y_2 = 0.25,
                  x_3 = 0.25, y_3 = -0.25,
                  x_4 = 0.25, y_4 = -0.25) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("x_"),
                        names_to = "xpos",
                        values_to = "x_shift") %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("y_"),
                        names_to = "ypos",
                        values_to = "y_shift") %>%
    dplyr::transmute(x = .data$x + .data$x_shift,
                     y = .data$y + .data$y_shift,
                     tiles,
                     scale_p) %>%
    dplyr::distinct()

  # Scale 3
  df_3 <- df %>%
    dplyr::filter(scale_p == 1/4) %>%
    dplyr::mutate(x_1 = -1/8 * 3, y_1 = 1/8 * 3,
                  x_2 = -1/8, y_2 = 1/8 * 3,
                  x_3 = 1/8, y_3 = 1/8 * 3,
                  x_4 = 1/8 * 3, y_4 = 1/8 * 3,
                  x_5 = -1/8 * 3, y_5 = 1/8,
                  x_6 = -1/8, y_6 = 1/8,
                  x_7 = 1/8, y_7 = 1/8,
                  x_8 = 1/8 * 3, y_8 = 1/8,
                  x_9 = -1/8 * 3, y_9 = -1/8 ,
                  x_10 = -1/8, y_10 = -1/8,
                  x_11 = 1/8, y_11 = -1/8,
                  x_12 = 1/8 * 3, y_12 = -1/8,
                  x_13 = -1/8 * 3, y_13 = -1/8 * 3,
                  x_14 = -1/8, y_14 = -1/8 * 3,
                  x_15 = 1/8, y_15 = -1/8 * 3,
                  x_16 = 1/8 * 3, y_16 = -1/8 * 3) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("x_"),
                        names_to = "xpos",
                        values_to = "x_shift") %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("y_"),
                        names_to = "ypos",
                        values_to = "y_shift") %>%
    dplyr::transmute(x = .data$x + .data$x_shift,
                     y = .data$y + .data$y_shift,
                     tiles,
                     scale_p) %>%
    dplyr::distinct()

  # Bind all scales
  df <- rbind(df_1,
              df_2,
              df_3)

  # Collect elements for assembling the mosaic
  x_c <- df$x
  y_c <- df$y
  type <- df$tiles
  scale_p <- df$scale_p

  mosaic <- purrr::pmap_dfr(list(x_c, y_c, type, scale_p), st_truchet_p)

  return(mosaic)
}
