ggradarpolygon <- function(
    data, 
    gridline.value = seq(20, 100, 20), 
    gridline.color = c(rep("gray", 4), "darkgray"), 
    gridline.linewidth = c(rep(1, 4), 2), 
    gridline.linetype = c(rep(2, 4), 1), 
    gridline.label.offset = -0.1 * max(gridline.value), 
    gridline.label.color = "darkgray", 
    gridline.label.size = 4, 
    gridline.label.family = NA,
    axis.label = colnames(data)[-1],
    axis.line.color = "darkgray", 
    axis.linewidth = 1,
    axis.label.offset = 1.1, 
    axis.label.size = 4,
    axis.label.color = "black", 
    axis.label.family = NA,
    radar.linewidth = 2,
    radar.point.size = 3,
    radar.color = NULL,
    radar.polygon = FALSE,
    radar.polygon.alpha = 0.5,
    title.name = NULL,
    title.name.size = 6.5,
    title.name.family = NULL,
    title.name.face = NULL,
    legend.position = "none",
    legend.title = NULL,
    legend.text.size = 9,
    legend.text.family = NULL,
    legend.text.face = NULL, 
    panel.background.color = "white", 
    plot.background.color = "white"
) {
  if (!require(RColorBrewer)) {
    install.packages("RColorBrewer")
  }
  library(RColorBrewer)

  two_pi <- 2 * pi
  n_axis <- axis.label |> 
    length()
  gridline_angle <- seq(0, two_pi, two_pi / n_axis)
  n_gridline_value <- length(gridline.value)
  n_gridline_angle <- length(gridline_angle)
  generate_path <- function (value, angle) {
    sin_angle <- sin(angle)
    cos_angle <- cos(angle)
    for (i in 1:length(value)) {
      value_i <- value[i]
      if(i == 1) {
        path <- tibble(x = value_i * sin_angle, y = value_i * cos_angle)
      } else {
        path <- path |> 
        add_row(x = value_i * sin_angle, y = value_i * cos_angle)
      }
    }
    path
  }
  gridline_path <- gridline.value |> 
    generate_path(gridline_angle) |> 
    mutate(group = rep(gridline.value, each = n_gridline_angle))
  gridline_color <- NULL
  gridline_linewidth <- NULL
  gridline_linetype <- NULL
  for (i in 1:n_gridline_value) {
    gridline_color <- c(
      gridline_color, 
      rep(gridline.color[i], n_gridline_angle)
      )
    gridline_linewidth <- c(
      gridline_linewidth, 
      rep(gridline.linewidth[i], n_gridline_angle)
    )
    gridline_linetype <- c(
      gridline_linetype, 
      rep(gridline.linetype[i], n_gridline_angle)
    )
  }
  ## label
  gridline_label <- tibble(
    x =  rep(gridline.label.offset, n_gridline_value), 
    y = gridline.value, 
    label = as.character(gridline.value)
  )
  
  # axis
  ## path
  axis_angle <- gridline_angle[-(n_axis + 1)] # exclude the last 
  # one since 2pi = 0
  maximum <- max(gridline.value)
  colnames(data)[1] <- "State"
  state <- data |> 
    pull(State)
  n_state <- state |> 
    length()
  axis_path <- c(0, maximum) |> 
    generate_path(axis_angle) |> 
    mutate(group = rep(1:n_axis, 2))
  ## label
  axis_label <- tibble(
    label =  axis.label,
    x =  maximum * axis.label.offset * sin(axis_angle), 
    y =  maximum * axis.label.offset * cos(axis_angle)
  )
  
  # radar
  radar_angle <- c(gridline_angle, 2 * pi)
  radar_value1 <- data |> 
    pull(2)
  radar_value <- data |> 
    select(2:length(data)) |> 
    mutate(value_last2 = radar_value1, value_last1 = radar_value1) |> 
    t()
  sin_radar_angle_n_state <- radar_angle |> 
    sin() |> 
    rep(n_state)
  cos_radar_angle_n_state <- radar_angle |> 
    cos() |> 
    rep(n_state)
  for (i in 1:length(radar_value)) {
    radar_value_i <- radar_value[i]
    sin_radar_angle_n_state_i <- sin_radar_angle_n_state[i]
    cos_radar_angle_n_state_i <- cos_radar_angle_n_state[i]
    if(i == 1) {
      radar_path <- tibble(
      x = radar_value_i * sin_radar_angle_n_state_i, 
      y = radar_value_i * cos_radar_angle_n_state_i
      )
    } else {
      radar_path <- radar_path |> 
      add_row(
        x = radar_value_i * sin_radar_angle_n_state_i, 
        y = radar_value_i * cos_radar_angle_n_state_i
      )
    }
  }
  radar_path <- radar_path |> 
    mutate(group = rep(as.character(state), each = length(radar_angle)))
  
  # plotting
  radar_plot <- ggplot() + 
    ## gridline
    geom_path(
      data = gridline_path, 
      aes(x, y, group = group), 
      color = gridline_color, 
      linewidth = gridline_linewidth,
      lty = gridline_linetype
    ) +
    geom_text(
      data = gridline_label, 
      aes(x, y, label = label), 
      size = gridline.label.size, 
      color = gridline.label.color, 
      family = gridline.label.family,
    ) +
    ## axis
    geom_path(
      data = axis_path, 
      aes(x, y, group = group), 
      color = axis.line.color, 
      linewidth = axis.linewidth
    ) +
    geom_text(
      data = axis_label, 
      aes(x, y, label = label), 
      size = axis.label.size, 
      color = axis.label.color, 
      family = axis.label.family, 
    ) +
    ## radar
    geom_point(
      data = radar_path, 
      aes(x, y, color = group),  
      size = radar.point.size
    ) +
    geom_path(
      data = radar_path, 
      aes(x, y, group = group, color = group), 
      linewidth = radar.linewidth
    ) + 
    scale_color_discrete(name = legend.title) +
    labs(x = NULL, y = NULL, title = title.name) +
    theme(
      axis.text = element_blank(), 
      axis.ticks = element_blank(), 
      aspect.ratio = 1, 
      legend.text = element_text(
        size = legend.text.size, 
        family = legend.text.family, 
        face = legend.text.face
      ), 
      legend.key.height = unit(0.2, "line"), 
      legend.key.width = unit(0.3, "line"), 
      legend.position = legend.position, 
      panel.background = element_rect(fill = panel.background.color), 
      panel.grid = element_blank(), 
      plot.background = element_rect(fill = plot.background.color), 
      plot.title = element_text(
        size = title.name.size, 
        family = title.name.family, 
        face = title.name.face
      ), 
    )
  if (!is.null(radar.color)) {
    radar_plot <- radar_plot +
      scale_color_manual(values = radar.color)
    if (isTRUE(radar.polygon)) {
      radar_plot <- radar_plot + 
        geom_polygon(
          data = radar_path,
          aes(x, y, group = group, fill = group),
          alpha = radar.polygon.alpha
        )  +
        scale_fill_manual(values = radar.color)
    } else {
      repetition <- n_state %/% 8
      modulo <- n_state %% 8
      color <- brewer.pal(n = 8, name = "Accent") |> 
        rep(repetition) |> 
        c(brewer.pal(n = modulo, name = "Accent"))
      radar_plot <- radar_plot +
        scale_color_manual(values = color)
      if (isTRUE(radar.polygon)) {
        radar_plot <- radar_plot + 
          geom_polygon(
            data = radar_path,
            aes(x, y, group = group, fill = group),
            alpha = radar.polygon.alpha
          ) +
          scale_fill_brewer(palette = "Accent")
      }
    } 
  }
  radar_plot
}
