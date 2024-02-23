#' Generate Dynamic Color Values
#'
#' This function dynamically generates a vector of color values based on the
#' number of groups. It uses RColorBrewer for smaller sets of groups and
#' generates a gradient for larger sets.
#'
#' @param num_groups The number of groups for which to generate color values.
#' @return A character vector of color values.
#' @importFrom RColorBrewer brewer.pal
#' @export
#' @examples
#' generate_color_values(5)
#' generate_color_values(20)
generate_color_values <- function(num_groups) {
  # Fallback colors for 1 or 2 groups
  fallback_colors <- c("#E41A1C", "#377EB8") # Adjust these colors as needed
  
  if (num_groups == 1) {
    # Return the first color if only one group is requested
    return(fallback_colors[1])
  } else if (num_groups == 2) {
    # Return the first two colors for two groups
    return(fallback_colors[1:2])
  } else if (num_groups <= max(RColorBrewer::brewer.pal.info$maxcolors)) {
    # Use RColorBrewer for 3 to max colors
    return(RColorBrewer::brewer.pal(num_groups, "Set3"))
  } else {
    # For more than the maximum supported colors in RColorBrewer, use a color ramp
    return(colorRampPalette(RColorBrewer::brewer.pal(8, "Set3"))(num_groups))
  }
}
