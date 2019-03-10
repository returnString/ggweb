
ggplot_to_chartjs <- function(src_plot) {
  built <- build_plot(src_plot)

  # FIXME: support for multiple layers
  data <- built$data[[1]]
  layer <- built$layers[[1]]
  layout <- built$layout
  plot <- built$plot

  # FIXME: currently unspecified geom_bars are incorrectly coming through as stacks
  layer_position_classes <- class(layer$position)
  position_type <- case_when(
    "PositionStack" %in% layer_position_classes ~ "stacked",
    "PositionDodge" %in% layer_position_classes ~ "dodged",
    T ~ "unknown"
  )

  geom_classes <- class(layer$geom)
  chart_type <- case_when(
    "GeomLine" %in% geom_classes ~ "line",
    any(c("GeomBar", "GeomCol") %in% geom_classes) ~ "bar",
    "GeomPoint" %in% geom_classes ~ "scatter"
  )

  x_scale_classes <- class(layout$panel_scales_x[[1]])
  x_scale_type <- case_when(
    "ScaleDiscrete" %in% x_scale_classes ~ "discrete",
    "ScaleContinuousDate" %in% x_scale_classes ~ "time",
    "ScaleContinuous" %in% x_scale_classes ~ "continuous",
    T ~ "unknown"
  )

  facet_classes <- class(layout$facet)
  facet_type <- case_when(
    "FacetWrap" %in% facet_classes ~ "wrap",
    "FacetGrid" %in% facet_classes ~ "grid",
    T ~ "none"
  )

  panel_params <- layout$panel_params[[1]]

  chart_type_options = list(
    line = list(
      lineTension = unbox(0),
      fill = unbox(F)
    )
  )

  tick_params <- function(source) {
    list(
      min = source[[1]],
      step = source[[2]] - source[[1]],
      max = source[[length(source)]]
    )
  }

  y_minor_ticks <- tick_params(panel_params[["y.minor_source"]])
  y_major_ticks <- tick_params(panel_params[["y.major_source"]])

  colour_params <- switch(chart_type,
                          line = list(border = "colour", background = "colour", single_border = T),
                          scatter = list(border = "colour", background = "colour", single_border = T),
                          bar = list(border = "fill", background = "fill", single_border = F)
  )

  # FIXME: not sure about these grouping conditions
  # discrete x axes are handled as ungrouped for now, need to fix this
  is_grouped <- length(unique(data$group)) > 1 && x_scale_type != "discrete"

  charts_by_panel <- lapply(split(data, data$PANEL), function(panel_data) {
    grouped_src_data <- if (is_grouped) split(panel_data, panel_data$group) else list(panel_data)

    datasets <- lapply(grouped_src_data, function(group_data) {
      colour_background <- gplots::col2hex(group_data[[colour_params$background]])
      colour_border <- gplots::col2hex(group_data[[colour_params$border]])

      if (colour_params$single_border) {
        colour_border <- unbox(colour_border[[1]])
      }

      for_rendering <- group_data

      if (x_scale_type == "time") {
        for_rendering$x <- for_rendering$x_src
      }

      if (position_type == "stacked") {
        for_rendering$y <- for_rendering$ymax - for_rendering$ymin
      }

      for_rendering %<>% select(x, y)

      size <- unbox(group_data$size[[1]] * ggplot2::.pt)

      options <- list(
        data = for_rendering,
        backgroundColor = colour_background,
        borderColor = colour_border,
        pointBorderColor = colour_border,
        pointBackgroundColor = colour_background,
        borderWidth = size,
        pointRadius = size,
        pointBorderWidth = size,
        pointHoverBorderWidth = size,
        pointHoverRadius = size * 1.5
      )

      if (is_grouped) {
        options$label <- unbox(group_data[[paste0(colour_params$background, "_src")]][[1]]) %>% trimws()
      }

      c(chart_type_options[[chart_type]], options)
    })

    is_stacked <- position_type == "stacked"
    panel_metadata <- built$panel_metadata %>% filter(PANEL == panel_data$PANEL[[1]])

    # for now, faceted plots just cat their col/row values if available
    panel_title <- if (facet_type != "none") {
      paste(c(panel_metadata$col_val[[1]], panel_metadata$row_val[[1]]) %>% trimws, collapse = ", ")
    } else {
      NA
    }

    if (is_stacked) {
      datasets %<>% rev()
    }

    list(
      type = unbox(chart_type),
      data = list(
        datasets = unname(datasets)
      ),
      options = list(
        repsonsive = unbox(T),
        maintainAspectRatio = unbox(F),
        title = list(
          display = unbox(!is.na(panel_title)),
          text = unbox(panel_title)
        ),
        animation = list(
          duration = unbox(0)
        ),
        tooltips = list(
          mode = unbox(ifelse(chart_type == "scatter", "point", "index")),
          intersect = unbox(F)
        ),
        legend = list(
          display = unbox(is_grouped),
          position = unbox("bottom"),
          reverse = unbox(is_stacked)
        ),
        # layout = list(
        #   padding = list(
        #     left = unbox(50),
        #     right = unbox(50)
        #   )
        # ),
        scales = list(
          xAxes = list(
            list(
              stacked = unbox(is_stacked),
              display = unbox(T),
              offset = unbox(T),
              type = unbox(switch(x_scale_type, time = "time", discrete = "category", continuous = "linear")),
              time = list(
                # FIXME: this ensures that days aren't rendered as times for low observation counts
                # should replace with proper handling for date vs time scales from ggplot
                minUnit = unbox("day")
              ),
              labels = panel_params$x.labels %>% trimws(),
              scaleLabel = list(
                display = unbox(T),
                labelString = unbox(plot$labels$x)
              ),
              ticks = list(
                autoSkip = unbox(x_scale_type != "discrete")
              )
            )
          ),
          yAxes = list(
            list(
              stacked = unbox(is_stacked),
              display = unbox(T),
              labels = panel_params$y.labels,
              scaleLabel = list(
                display = unbox(T),
                labelString = unbox(plot$labels$y)
              ),
              ticks = list(
                suggestedMin = unbox(y_major_ticks$min),
                suggestedMax = unbox(y_major_ticks$max),
                stepSize = unbox(y_major_ticks$step)
              )
            )
          )
        )
      )
    )
  })

  columns <- switch(facet_type,
                    grid = max(length(unique(built$panel_metadata$col_val)), 1),
                    wrap = length(charts_by_panel),
                    none = 1)

  rows <- switch(facet_type,
                 grid = max(length(unique(built$panel_metadata$row_val)), 1),
                 wrap = 1,
                 none = 1)

  title <- if ("title" %in% names(plot$labels)) {
    plot$labels$title
  } else {
    NA
  }

  payload <- list(
    panels = unname(charts_by_panel),
    panelLayout = list(
      type = unbox(facet_type),
      columns = unbox(columns),
      rows = unbox(rows)
    ),
    title = unbox(title)
  )

  jsonlite::toJSON(payload) %>% as.character
}
