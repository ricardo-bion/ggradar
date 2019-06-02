#' Calculate Axis Path
#'
#' Calculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
#'
#' @param var.names list of variables to be plotted on radar plot
#' @param min MININUM value required for the plotted axes (same value will be applied to all axes)
#' @param max MAXIMUM value required for the plotted axes (same value will be applied to all axes)
#'
#' @return a dataframe of the calculated axis paths
CalculateAxisPath <- function(var.names, min, max) {
  # var.names <- c("v1","v2","v3","v4","v5")
  n.vars <- length(var.names) # number of vars (axes) required
  # Cacluate required number of angles (in radians)
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / n.vars)
  # calculate vectors of min and max x+y coords
  min.x <- min * sin(angles)
  min.y <- min * cos(angles)
  max.x <- max * sin(angles)
  max.y <- max * cos(angles)
  # Combine into a set of uniquely numbered paths (one per variable)
  axisData <- NULL
  for (i in 1:n.vars) {
    a <- c(i, min.x[i], min.y[i])
    b <- c(i, max.x[i], max.y[i])
    axisData <- rbind(axisData, a, b)
  }
  # Add column names + set row names = row no. to allow conversion into a data frame
  colnames(axisData) <- c("axis.no", "x", "y")
  rownames(axisData) <- seq(1:nrow(axisData))
  # Return calculated axis paths
  as.data.frame(axisData)
}
