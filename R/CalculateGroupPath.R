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
  # Drop dead levels. This might happen if the data is filtered on the way
  # into ggradar.
  path <- forcats::fct_drop(df[, 1])
  # set the name of the variable that will be used for grouping
  theGroupName <- colnames(df)[1]
  
  ## find increment
  nPathPoints <- ncol(df) - 1
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / nPathPoints)
  ## create graph data frame
  nDataPoints <- ncol(df) * length(levels(path))
  graphData <- data.frame(
                          seg = rep("",nDataPoints),
                          x = rep(0, nDataPoints),
                          y = rep(0, nDataPoints))
  colnames(graphData)[1] <- theGroupName

  rowNum <- 1
  for (i in 1:length(levels(path))) {
    pathData <- subset(df, df[, 1] == levels(path)[i])
    for (j in c(2:ncol(df))) {
      graphData[rowNum,theGroupName] <- levels(path)[i]
      graphData$x[rowNum] <- pathData[, j] * sin(angles[j - 1])
      graphData$y[rowNum] <- pathData[, j] * cos(angles[j - 1])
      rowNum <- rowNum + 1
    }
    ## complete the path by repeating first pair of coords in the path
    graphData[rowNum,theGroupName] <- levels(path)[i]
    graphData$x[rowNum] <- pathData[, 2] * sin(angles[1])
    graphData$y[rowNum] <- pathData[, 2] * cos(angles[1])
    rowNum <- rowNum + 1
  }
  # Make sure that name of first column matches that of input data (in case !="group")
  graphData[,1] <- factor(graphData[,1], levels=levels(path) ) # keep group order
  graphData # data frame returned by function
}
