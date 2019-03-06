# principally lifted from Plotly R codebase as an alternative to ggplot_build
# https://github.com/ropensci/plotly (MIT)

ggfun <- function(x) tryCatch(utils::getFromNamespace(x, "ggplot2"), error = function(e) NULL)

build_plot <- function(plot) {
  plot <- ggfun("plot_clone")(plot)
  if (length(plot$layers) == 0) {
    plot <- plot + ggplot2::geom_blank()
  }

  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$layer_data(plot$data))

  scales <- plot$scales

  # Apply function to layer and matching data
  by_layer <- function(f) {
    out <- vector("list", length(data))
    for (i in seq_along(data)) {
      out[[i]] <- f(l = layers[[i]], d = data[[i]])
    }
    out
  }

  # Initialise panels, add extra data for margins & missing facetting
  # variables, and add on a PANEL variable to data
  layout <- ggfun("create_layout")(plot$facet, plot$coordinates)
  data <- layout$setup(layer_data, plot$data, plot$plot_env)

  # save the domain of the group for display in tooltips
  groupDomains <- Map(function(x, y) {
    aes_g <- y$mapping[["group"]]# %||% plot$mapping[["group"]]
    tryCatch(rlang::eval_tidy(aes_g, x), error = function(e) NULL)
  }, data, layers)

  panel_data_samples <- data[[1]] %>%
    group_by(PANEL) %>%
    filter(row_number() == 1) %>%
    ungroup()

  panel_metadata <- panel_data_samples %>% select(PANEL)

  if (!is.null(plot$facet$params$facets)) {
    panel_metadata$col_val <- rlang::eval_tidy(plot$facet$params$facets[[1]], panel_data_samples)
  } else if (!is.null(plot$facet$params$rows)) {
    if (length(plot$facet$params$rows) == 1) {
      panel_metadata$row_val <- rlang::eval_tidy(plot$facet$params$rows[[1]], panel_data_samples)
    }
    if (length(plot$facet$params$cols) == 1) {
      panel_metadata$col_val <- rlang::eval_tidy(plot$facet$params$cols[[1]], panel_data_samples)
    }
  }

  # Compute aesthetics to produce data with generalised variable names
  data <- by_layer(function(l, d) l$compute_aesthetics(d, plot))

  # add frame to group if it exists
  data <- lapply(data, function(d) {
    if (!"frame" %in% names(d)) return(d)
    d$group <- with(d, paste(group, frame, sep = "-"))
    d
  })

  # The computed aesthetic codes the groups as integers
  # Here we build a map each of the integer values to the group label
  group_maps <- Map(function(x, y) {
    tryCatch({
      x_group <- x[["group"]]
      names(x_group) <- y
      x_group <- x_group[!duplicated(x_group)]
      x_group
    }, error = function(e) NULL
    )
  }, data, groupDomains)

  # Before mapping x/y position, save the domain (for discrete scales)
  # to display in tooltip.
  data <- lapply(data, function(d) {
    d[["x_src"]] <- d[["x"]]
    d[["y_src"]] <- d[["y"]]
    d
  })

  # Transform all scales
  data <- lapply(data, ggfun("scales_transform_df"), scales = scales)

  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")

  layout$train_position(data, scale_x(), scale_y())

  data <- layout$map_position(data)

  # build a mapping between group and key
  # if there are multiple keys within a group, the key is a list-column
  reComputeGroup <- function(x, layer = NULL) {
    # 1-to-1 link between data & visual marks -- group == key
    if (inherits(layer$geom, "GeomDotplot")) {
      x <- split(x, x[["PANEL"]])
      x <- lapply(x, function(d) {
        d[["group"]] <- do.call("order", d[c("x", "group")])
        d
      })
      x <- dplyr::bind_rows(x)
    }
    if (inherits(layer$geom, "GeomSf")) {
      x <- split(x, x[["PANEL"]])
      x <- lapply(x, function(d) {
        d[["group"]] <- seq_len(nrow(d))
        d
      })
      # I think this is safe?
      x <- suppressWarnings(dplyr::bind_rows(x))
    }
    x
  }

  # for some geoms (e.g. boxplots) plotly.js needs the "pre-statistics" data
  # we also now provide the option to return one of these two
  prestats_data <- data
  data <- by_layer(function(l, d) l$compute_statistic(d, layout))
  data <- by_layer(function(l, d) l$map_statistic(d, plot))

  # Make sure missing (but required) aesthetics are added
  ggfun("scales_add_missing")(plot, c("x", "y"), plot$plot_env)

  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- by_layer(function(l, d) l$compute_geom_1(d))

  # compute_geom_1 can reorder the rows from `data`, making groupDomains
  # invalid. We rebuild groupDomains based on the current `data` and the
  # group map we built before.
  groupDomains <- Map(function(x, y) {
    tryCatch({
      names(y)[match(x$group, y)]
    }, error = function(e) NULL
    )
  }, data, group_maps)

  # there are some geoms (e.g. geom_dotplot()) where attaching the key
  # before applying the statistic can cause problems, but there is still a
  # 1-to-1 corresponding between graphical marks and

  # Apply position adjustments
  data <- by_layer(function(l, d) l$compute_position(d, layout))

  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what's
  # displayed, or does it include the range of underlying data
  layout$reset_scales()
  layout$train_position(data, scale_x(), scale_y())
  layout$setup_panel_params()
  data <- layout$map_position(data)

  # Train and map non-position scales
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, ggfun("scales_train_df"), scales = npscales)
    # this for loop is unique to plotly -- it saves the "domain"
    # of each non-positional scale for display in tooltips
    for (sc in npscales$scales) {
      data <- lapply(data, function(d) {
        # scale may not be relevant for every layer data
        if (any(names(d) %in% sc$aesthetics)) {
          d[paste0(sc$aesthetics, "_src")] <- d[sc$aesthetics]
        }
        d
      })
    }
    data <- lapply(data, ggfun("scales_map_df"), scales = npscales)
  }

  # Fill in defaults etc.
  data <- by_layer(function(l, d) l$compute_geom_2(d))

  # Let layer stat have a final say before rendering
  data <- by_layer(function(l, d) l$finish_statistics(d))

  # Let Layout modify data before rendering
  data <- layout$finish_data(data)

  list(data = data, layers = layers, layout = layout, plot = plot, panel_metadata = panel_metadata)
}
