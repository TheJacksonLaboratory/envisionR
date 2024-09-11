#' @export
group_mean_sem_ribbon_plot <- function(activity_data,
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
                                       occupancy_weight = TRUE,
                                       na.rm = TRUE,
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
  cage_name <- animals_cage_quantity <- visualize <- group <- var_col <-
    group_name <- tzone <- mean_activity <- ll_activity <- ul_activity <-
    ymin <- ymax <- NULL

  stopifnot("data is not a tibble" = tibble::is_tibble(activity_data))
  stopifnot(
    "the column title in yvar is not in activity_data" =
      yvar %in% colnames(activity_data)
  )

  activity_data[, "var_col"] <- activity_data[, yvar]

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
      plotcolors <- colors[seq_len(length(unique(dplyr::pull(activity_data, "visualize"))))]
    } else {
      plotcolors <- colors
    }
  }

  if (is.null(metadata)) {
    plot_title <- NULL
  } else if (metadata[["study_name"]] != "") {
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
          dplyr::mutate(ymin = -Inf, ymax = Inf)
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
          dplyr::mutate(ymin = -Inf, ymax = Inf)
      }
    }
  }

  group_summary <- group_level_mean_sem(
    activity_data = activity_data,
    var = "var_col",
    by = "start",
    occupancy_normalize = occupancy_norm,
    occupancy_weighted = occupancy_weight,
    na.rm = na.rm
  )

  if (is.null(xlims)) {
    xminmax <- range(group_summary |> dplyr::pull(start), na.rm = TRUE)
    xrange <- xminmax[2] - xminmax[1]
    xminmax_plot <- xminmax
  } else {
    stopifnot(inherits(xlims, "POSIXct"))
    stopifnot(length(xlims) == 2)
    stopifnot(xlims[1] < xlims[2])
    xminmax_plot <- xlims
  }

  if (is.null(ylims)) {
    yminmax <- range(c(
      group_summary |>
        dplyr::filter(start >= xminmax_plot[1] &
          start <= xminmax_plot[2]) |>
        dplyr::pull(mean_activity),
      group_summary |>
        dplyr::filter(start >= xminmax_plot[1] &
          start <= xminmax_plot[2]) |>
        dplyr::pull(ll_activity),
      group_summary |>
        dplyr::filter(start >= xminmax_plot[1] &
          start <= xminmax_plot[2]) |>
        dplyr::pull(ul_activity)
    ))
    yrange <- yminmax[2] - yminmax[1]
    yminmax_plot <- yminmax + c(-0.05, 0.05) * yrange
  } else {
    stopifnot(is.numeric(ylims))
    stopifnot(length(ylims) == 2)
    stopifnot(ylims[1] < ylims[2])
    yminmax_plot <- ylims
  }

  summaryplot <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = lightdark_df,
      ggplot2::aes(
        xmin = lights_off,
        xmax = lights_on,
        ymin = ymin,
        ymax = ymax
      ),
      fill = darkphase_col,
      alpha = darkphase_alpha
    ) +
    ggplot2::geom_ribbon(
      data = group_summary,
      ggplot2::aes(
        x = start,
        ymin = ll_activity,
        ymax = ul_activity,
        fill = group_name
      ),
      color = NA,
      alpha = 0.5,
    ) +
    ggplot2::geom_line(
      data = group_summary,
      ggplot2::aes(
        x = start,
        y = mean_activity,
        color = group_name
      )
    ) +
    ggplot2::scale_color_manual(
      name = legendtitle,
      values = plotcolors
    ) +
    ggplot2::scale_fill_manual(
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
    summaryplot <- summaryplot +
      ggplot2::facet_wrap(. ~ group_name,
        ncol = 1,
        as.table = FALSE
      )
  }

  if (!is.null(y_axis_label)) {
    stopifnot(is.character(y_axis_label))
    stopifnot(length(y_axis_label) == 1)
    summaryplot <- summaryplot +
      ggplot2::ylab(y_axis_label)
  }

  if (!is.null(x_axis_label)) {
    stopifnot(is.character(x_axis_label))
    stopifnot(length(x_axis_label) == 1)
    summaryplot <- summaryplot +
      ggplot2::xlab(x_axis_label)
  }

  return(summaryplot)
}
