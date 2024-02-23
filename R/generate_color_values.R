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
  if (num_groups <= max(brewer.pal.info$maxcolors)) {
    return(brewer.pal(num_groups, "Set3"))
  } else {
    return(colorRampPalette(brewer.pal(8, "Set3"))(num_groups))
  }
}