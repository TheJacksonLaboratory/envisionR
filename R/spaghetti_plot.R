#' @export
spaghetti_plot <- function(activity_data,
                           metadata = NULL,
                           lights_on = NULL,
                           lights_off = NULL,
                           visualize_on = "group",
                           xvar = "start",
                           yvar = NULL,
                           colors = NULL,
                           lightdark = TRUE,
                           darkphase_col = "#CCCCCC",
                           darkphase_alpha = 0.5,
                           facet = TRUE,
                           occupancy_norm = TRUE,
                           x_axis_label = NULL,
                           y_axis_label = NULL,
                           xlims = NULL,
                           ylims = NULL,
                           quietly = FALSE) {
  # Getting required packages.
  stopifnot(requireNamespace("ggplot2"))
  stopifnot(requireNamespace("tibble"))
  stopifnot(requireNamespace("dplyr"))
  stopifnot(requireNamespace("scales"))

  # binding global variables
  cage_name <- animals_cage_quantity <- visualize <- group <- var_col <- NULL

  stopifnot("data is not a tibble" = tibble::is_tibble(activity_data))
  stopifnot(
    "the column title in yvar is not in activity_data" =
      yvar %in% colnames(activity_data)
  )

  activity_data <- as.data.frame(activity_data)

  dataset_occunorm_col <- grep("^occupancy_norm",
    colnames(activity_data),
    value = TRUE
  )

  if (length(dataset_occunorm_col) != 0) {
    is_normalized <- sum(activity_data[, dataset_occunorm_col]) == nrow(activity_data)
  } else {
    is_normalized <- FALSE
  }

  if (occupancy_norm) {
    if (is_normalized) {
      activity_data[, "var"] <- activity_data[, var]
      if (!quietly) {
        warning("dataset already occupancy normalized")
      }
    } else {
      activity_data[, "var_col"] <- ifelse(activity_data[, "animals_cage_quantity"] != 0,
        activity_data[, yvar] / activity_data[, "animals_cage_quantity"],
        NA
      )
    }
  } else {
    activity_data[, "var_col"] <- activity_data[, yvar]
  }

  if (xvar == "start") {
    activity_data <- activity_data |>
      dplyr::mutate(timevar = start)
  } else {
    stopifnot(xvar %in% colnames(activity_data))
    activity_data[, "timevar"] <- activity_data[, xvar]
  }

  if (visualize_on == "group") {
    activity_data <- activity_data |>
      dplyr::group_by(cage_name) |>
      dplyr::mutate(visualize = group_name)
    legendtitle <- "Group"
  }

  if (is.null(colors)) {
    plotcolors <- scales::hue_pal()(length(unique(dplyr::pull(activity_data, "visualize"))))
  } else {
    if (length(colors) < length(unique(dplyr::pull(activity_data, "visualize")))) {
      stop("insufficient number of colors provided")
    } else if (length(colors) > length(unique(dplyr::pull(activity_data, "visualize"))) & !quietly) {
      warning("more colors provided than number of grouping variables")
    } else {
      plotcolors <- colors
    }
  }

  if (!is.null(metadata) & metadata[["study_name"]] != "") {
    plot_title <- paste0(metadata[["study_name"]])
  } else {
    plot_title <- NULL
  }

  if (lightdark) {
    date_range <- range(as.Date(as.POSIXct(as.character(activity_data |> dplyr::pull(start)),
      tz = "UTC"
    )))
    if (!is.null(metadata)) {
      if (!is.na(metadata[["lights_on"]]) & !is.na(metadata[["lights_off"]])) {
        tzone_md <- ifelse(is.na(metadata[["tzone"]]),
          unique(activity_data |> dplyr::pull(tzone)),
          metadata[["tzone"]]
        )
        lightson_time <- gsub(":\\d{2}$", "", as.character(metadata[["lights_on"]]))
        lightsoff_time <- gsub(":\\d{2}$", "", as.character(metadata[["lights_off"]]))
        lightdark_df <- lightdark_times(
          lights_on_time = lightson_time,
          lights_off_time = lightsoff_time,
          start_date = date_range[1],
          end_date = date_range[2],
          tzone = tzone_md
        ) |>
          mutate(ymin = -Inf, ymax = Inf)
      }
    } else {
      if (is.null(lights_on) | is.null(lights_off)) {
        stop("metadata is not provided and lights_on or lights_off is not given")
      } else {
        tzone_md <- unique(activity_data |> dplyr::pull(tzone))
        # Will need to implement error checking for lights-on and lights-off
        lightdark_df <- lightdark_times(
          lights_on_time = lights_on,
          lights_off_time = lights_off,
          start_date = date_range[1],
          end_date = date_range[2],
          tzone = tzone_md
        ) |>
          mutate(ymin = -Inf, ymax = Inf)
      }
    }
  }

  if (is.null(xlims)) {
    xminmax <- range(activity_data |> dplyr::pull(timevar), na.rm = TRUE)
    xrange <- xminmax[2] - xminmax[1]
    xminmax_plot <- xminmax
  } else {
    stopifnot(is.POSIXct(xlims))
    stopifnot(length(xlims) == 2)
    stopifnot(xlims[1] < xlims[2])
    xminmax_plot <- xlims
  }

  if (is.null(ylims)) {
    yminmax <- range(activity_data |>
      dplyr::filter(timevar >= xminmax_plot[1] &
        timevar <= xminmax_plot[2]) |>
      dplyr::pull(var_col), na.rm = TRUE)
    yrange <- yminmax[2] - yminmax[1]
    yminmax_plot <- yminmax + c(-0.05, 0.05) * yrange
  } else {
    stopifnot(is.numeric(ylims))
    stopifnot(length(ylims) == 2)
    stopifnot(ylims[1] < ylims[2])
    yminmax_plot <- ylims
  }

  spaghettiplot <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = lightdark_df,
      aes(
        xmin = lights_off,
        xmax = lights_on,
        ymin = ymin,
        ymax = ymax
      ),
      fill = darkphase_col,
      alpha = darkphase_alpha
    ) +
    ggplot2::geom_line(
      data = activity_data,
      ggplot2::aes(
        x = timevar,
        y = var_col,
        color = visualize,
        group = cage_name
      )
    ) +
    ggplot2::scale_color_manual(
      name = legendtitle,
      values = plotcolors
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom"
    ) +
    ggplot2::coord_cartesian(
      xlim = xminmax_plot,
      ylim = yminmax_plot
    ) +
    ggplot2::xlab("Time") +
    ggplot2::ylab(yvar) +
    ggplot2::ggtitle(plot_title)

  if (facet) {
    spaghettiplot <- spaghettiplot +
      ggplot2::facet_wrap(. ~ visualize,
        ncol = 1,
        as.table = FALSE
      )
  }

  if (!is.null(y_axis_label)) {
    stopifnot(is.character(y_axis_label))
    stopifnot(length(y_axis_label) == 1)
    spaghettiplot <- spaghettiplot +
      ggplot2::ylab(y_axis_label)
  }

  if (!is.null(x_axis_label)) {
    stopifnot(is.character(x_axis_label))
    stopifnot(length(x_axis_label) == 1)
    spaghettiplot <- spaghettiplot +
      ggplot2::xlab(x_axis_label)
  }

  return(spaghettiplot)
}
