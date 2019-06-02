#' Generate circle coordinates
#' 
#' Generate coordinates to draw a circle. 
#'
#' @param center coordinate for centroid
#' @param r radius
#' @param npoints number of coordinates to generate
#'
#' @return a dataframe
#' @source 
#' Adapted from Joran's response to \url{http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2}.
funcCircleCoords <- function(center = c(0, 0), r = 1, npoints = 100) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
