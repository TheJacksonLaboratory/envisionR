double_plot <- function(dataset,
                        measure,
                        ylab,
                        group_vars = "group_name",
                        aggregate_by_cage = TRUE,
                        dark_start = "18:00:00",
                        dark_end = "06:00:00",
                        zt0_time = "06:00:00",
                        color = ggokabeito::palette_okabe_ito(),
                        start_datetime = NULL,
                        stop_datetime = NULL,
                        tzone = "US/Eastern",
                        order = 1:9,
                        build_plot = TRUE) {

  # Convert to data frame and set up measure column
  dataset <- dataset |>
    as.data.frame()
  dataset$measure <- dataset[, measure]

  if (!is.null(start_datetime)) {
    start_datetime = lubridate::ymd_hms(start_datetime, tz = tzone)
    dataset = dataset |>
      dplyr::filter(start >= start_datetime)
  }

  if (!is.null(stop_datetime)) {
    stop_datetime = lubridate::ymd_hms(stop_datetime, tz = tzone)
    dataset = dataset |>
      dplyr::filter(start <= stop_datetime)
  }

  # Detect time bins automatically from data
  time_bins <- dataset |>
    dplyr::pull(start_time_local) |>
    unique() |>
    sort()

  n_bins <- length(time_bins)
  time_labels <- format(time_bins, "%H:%M")
  offset = which(time_labels == zt0_time)
  time_labels = time_labels[c(offset:length(time_labels),1:(offset-1))]

  # Aggregate data based on flag
  if (aggregate_by_cage) {
    # Two-stage: cage means, then group means of cage means
    dataset_ribbon <- dataset |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "start_time_local", "cage_name")))) |>
      dplyr::summarize(mean_measure_over_time = mean(measure, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "start_time_local")))) |>
      dplyr::summarize(mean_measure = mean(mean_measure_over_time, na.rm = TRUE),
                       sd_measure = sd(mean_measure_over_time, na.rm = TRUE),
                       n_measure = sum(!is.na(mean_measure_over_time)),
                       sem_measure = sd_measure / sqrt(n_measure),
                       ll_measure = mean_measure - sem_measure,
                       ul_measure = mean_measure + sem_measure,
                       .groups = "drop")
  } else {
    # Direct: individual observations to group means
    dataset_ribbon <- dataset |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "start_time_local")))) |>
      dplyr::summarize(mean_measure = mean(measure, na.rm = TRUE),
                       sd_measure = sd(measure, na.rm = TRUE),
                       n_measure = sum(!is.na(measure)),
                       sem_measure = sd_measure / sqrt(n_measure),
                       ll_measure = mean_measure - sem_measure,
                       ul_measure = mean_measure + sem_measure,
                       .groups = "drop")
  }

  # Create numeric index for plotting
  dataset_ribbon <- dataset_ribbon |>
    dplyr::mutate(
      start_time = factor(format(start_time_local, "%H:%M"),
                          levels = time_labels,
                          ordered = TRUE),
      start_time_num = as.numeric(start_time)
    )

  # Create double plot: duplicate data with offset, wrap first point to end
  dataset_ribbon_dp1 <- dataset_ribbon |>
    dplyr::mutate(start_time_num = start_time_num + n_bins)

  dataset_ribbon_dp2 <- dataset_ribbon |>
    dplyr::filter(start_time_num == 1) |>
    dplyr::mutate(start_time_num = 2 * n_bins + 1)

  dataset_ribbon_dp <- rbind(dataset_ribbon,
                             dataset_ribbon_dp1,
                             dataset_ribbon_dp2)

  # Calculate dark period rectangle positions
  dark_start_hm <- which(as.character(dataset_ribbon_dp$start_time) == dark_start)
  dark_end_hm <- which(as.character(dataset_ribbon_dp$start_time) == dark_end)

  dark_start_idx <- dataset_ribbon_dp[dark_start_hm,] |>
    dplyr::pull(start_time_num) |>
    unique()
  dark_end_idx <- dataset_ribbon_dp[dark_end_hm,] |>
    dplyr::pull(start_time_num) |>
    unique()
  dark_end_idx = dark_end_idx[-1]

  # Generate dark period rectangles (handles wraparound)
  dark_rects <- data.frame(xmin = dark_start_idx, xmax = dark_end_idx,
                           ymin = -Inf, ymax = Inf)

  # Calculate ZT for lights-off
  zt0_time_obj <- hms::as_hms(dark_end)
  dark_start_time_obj <- hms::as_hms(dark_start)
  diff_sec = as.numeric(dark_start_time_obj - zt0_time_obj)
  diff_min = diff_sec / 60
  diff_hrs = diff_sec / 3600

  zt_lights_off <- diff_hrs

  # Find indices for key timepoints
  zt0_idx <- match(dark_end, time_labels)
  noon_idx <- match("12:00:00", time_labels)
  lights_off_idx <- match(dark_start, time_labels)
  midnight_idx <- match("00:00:00", time_labels)

  # Build breaks and labels for key timepoints across both cycles
  axis_breaks <- c()
  axis_labels <- c()

  # First cycle
  if (!is.na(zt0_idx)) {
    axis_breaks <- c(axis_breaks, zt0_idx)
    axis_labels <- c(axis_labels, paste0(gsub("\\:00$","",time_labels[zt0_idx]), "\n(ZT0)"))
  }
  if (!is.na(noon_idx)) {
    axis_breaks <- c(axis_breaks, noon_idx)
    axis_labels <- c(axis_labels, "12:00\n(Noon)")
  }
  if (!is.na(lights_off_idx)) {
    axis_breaks <- c(axis_breaks, lights_off_idx)
    axis_labels <- c(axis_labels, paste0(gsub("\\:00$","",time_labels[lights_off_idx]), "\n(ZT", zt_lights_off, ")"))
  }
  if (!is.na(midnight_idx)) {
    axis_breaks <- c(axis_breaks, midnight_idx)
    axis_labels <- c(axis_labels, "00:00\n(Midnight)")
  }

  # Second cycle (offset by n_bins)
  if (!is.na(zt0_idx)) {
    axis_breaks <- c(axis_breaks, zt0_idx + n_bins)
    axis_labels <- c(axis_labels, paste0(gsub("\\:00$","",time_labels[zt0_idx]), "\n(ZT0)"))
  }
  if (!is.na(noon_idx)) {
    axis_breaks <- c(axis_breaks, noon_idx + n_bins)
    axis_labels <- c(axis_labels, "12:00\n(Noon)")
  }
  if (!is.na(lights_off_idx)) {
    axis_breaks <- c(axis_breaks, lights_off_idx + n_bins)
    axis_labels <- c(axis_labels, paste0(gsub("\\:00$","",time_labels[lights_off_idx]), "\n(ZT", zt_lights_off, ")"))
  }
  if (!is.na(midnight_idx)) {
    axis_breaks <- c(axis_breaks, midnight_idx + n_bins)
    axis_labels <- c(axis_labels, "00:00\n(Midnight)")
  }

  # Add final ZT0 at the end
  if (!is.na(zt0_idx)) {
    axis_breaks <- c(axis_breaks, 2 * n_bins + 1)
    axis_labels <- c(axis_labels, paste0(gsub("\\:00$","",time_labels[zt0_idx]), "\n(ZT0)"))
  }

  # Build plot
  if (build_plot) { p <- ggplot() +
    geom_rect(data = dark_rects, aes(xmin = xmin,
                                     xmax = xmax,
                                     ymin = ymin,
                                     ymax = ymax),
              fill = "#DDDDDD",
              color = NA) +
    geom_ribbon(data = dataset_ribbon_dp,
                aes(x = start_time_num,
                    ymin = ll_measure,
                    ymax = ul_measure,
                    fill = group_name),
                color = NA, alpha = 0.5) +
    geom_line(data = dataset_ribbon_dp,
              aes(x = start_time_num,
                  y = mean_measure,
                  color = group_name)) +
    envisionJAX::theme_jax_pub() +
    ggokabeito::scale_color_okabe_ito(name = "Genotype", order = order) +
    ggokabeito::scale_fill_okabe_ito(name = "Genotype", order = order) +
    scale_x_continuous(breaks = axis_breaks,
                       labels = axis_labels) +
    xlab("Time of Day") +
    ylab(ylab) +
    theme(legend.position = "bottom")
  } else {
    p = list(df = dataset_ribbon_dp,
             ld = dark_rects,
             breaks = axis_breaks,
             labels = axis_labels)
  }

  return(p)
}
