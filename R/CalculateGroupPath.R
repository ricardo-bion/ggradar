#' Calculate Group Path
#'
#' Converts variable values into a set of radial x-y coordinates
#'
#' @param df a dataframe with Col 1 is group ('unique' cluster / group ID of entity) and Col 2-n are  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
#'
#' @return a dataframe of the calculated axis paths
#' 
#' @source 
#' Code adapted from a solution posted by Tony M to \url{http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r}.
CalculateGroupPath <- function(df) {
  path <- df[, 1]
  
  ## find increment
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / (ncol(df) - 1))
  ## create graph data frame
  graphData <- data.frame(seg = "", x = 0, y = 0)
  graphData <- graphData[-1, ]
  
  for (i in levels(path)) {
    pathData <- subset(df, df[, 1] == i)
    for (j in c(2:ncol(df))) {
      # pathData[,j]= pathData[,j]
      
      
      graphData <- rbind(graphData, data.frame(
        group = i,
        x = pathData[, j] * sin(angles[j - 1]),
        y = pathData[, j] * cos(angles[j - 1])
      ))
    }
    ## complete the path by repeating first pair of coords in the path
    graphData <- rbind(graphData, data.frame(
      group = i,
      x = pathData[, 2] * sin(angles[1]),
      y = pathData[, 2] * cos(angles[1])
    ))
  }
  # Make sure that name of first column matches that of input data (in case !="group")
  colnames(graphData)[1] <- colnames(df)[1]
  graphData # data frame returned by function
}
