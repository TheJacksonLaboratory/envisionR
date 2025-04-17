# unit tests for spaghetti plot

library("dplyr")
library("hms")


# setup for test
metadata <- envision_metadata(
  study_name = "A",
  tzone = "US/Pacific",
  lights_on = "06:00:00",
  lights_off = "18:00:00",
  study_url = "https://envision.jax.org/org/1001/study/1002/overview"
)

activity_data <- activity_cage_data_example |>
  dplyr::mutate(
    var_col = movement_mean_per_cage_cm_s_hour / animals_cage_quantity,
    timevar = start,
    visualize = group_name
  )

activity_data2 <- activity_cage_data_example |>
  dplyr::mutate(
    var_col = movement_mean_per_cage_cm_s_hour,
    timevar = start,
    visualize = group_name
  )

lightson_time <- gsub(
  ":\\d{2}$", "",
  as.character(metadata[["lights_on"]])
)
lightsoff_time <- gsub(
  ":\\d{2}$", "",
  as.character(metadata[["lights_off"]])
)
date_range <- range(as.Date(as.POSIXct(
  as.character(activity_data |>
    dplyr::pull(timevar)),
  tz = "UTC"
)))

lightdark_df <- lightdark_times(
  lights_on_time = lightson_time,
  lights_off_time = lightsoff_time,
  start_date = date_range[[1]],
  end_date = date_range[[2]],
  tzone = metadata[["tzone"]]
) |>
  dplyr::mutate(ymin = -Inf, ymax = Inf)

plotcolors <- scales::hue_pal()(length(unique(dplyr::pull(activity_data, "visualize"))))

xminmax <- range(activity_data |> dplyr::pull(timevar), na.rm = TRUE)
xrange <- xminmax[2] - xminmax[1]
xminmax_plot <- xminmax

yminmax <- range(activity_data |>
  dplyr::filter(timevar >= xminmax_plot[1] &
    timevar <= xminmax_plot[2]) |>
  dplyr::pull(var_col), na.rm = TRUE)
yrange <- yminmax[2] - yminmax[1]
yminmax_plot <- yminmax + c(-0.05, 0.05) * yrange

yminmax2 <- range(activity_data2 |>
  dplyr::filter(timevar >= xminmax_plot[1] &
    timevar <= xminmax_plot[2]) |>
  dplyr::pull(var_col), na.rm = TRUE)
yrange2 <- yminmax2[2] - yminmax2[1]
yminmax_plot2 <- yminmax2 + c(-0.05, 0.05) * yrange2

spaghettiplot_base <- ggplot2::ggplot() +
  ggplot2::geom_rect(
    data = lightdark_df,
    ggplot2::aes(
      xmin = lights_off,
      xmax = lights_on,
      ymin = ymin,
      ymax = ymax
    ),
    fill = "#CCCCCC",
    alpha = 0.5
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
    name = "Group",
    values = plotcolors
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom"
  )


spaghettiplot_example <- spaghettiplot_base +
  ggplot2::coord_cartesian(
    xlim = xminmax_plot,
    ylim = yminmax_plot
  ) +
  ggplot2::xlab("Time") +
  ggplot2::ylab("movement_mean_per_cage_cm_s_hour") +
  ggplot2::ggtitle(metadata[["study_name"]])

custom_ylim <- c(-10, 10)

spaghettiplot_example_customy <- spaghettiplot_base +
  ggplot2::coord_cartesian(
    xlim = xminmax_plot,
    ylim = custom_ylim
  ) +
  ggplot2::xlab("Time") +
  ggplot2::ylab("movement_mean_per_cage_cm_s_hour") +
  ggplot2::ggtitle(metadata[["study_name"]])

spaghettiplot_example2 <- ggplot2::ggplot() +
  ggplot2::geom_rect(
    data = lightdark_df,
    ggplot2::aes(
      xmin = lights_off,
      xmax = lights_on,
      ymin = ymin,
      ymax = ymax
    ),
    fill = "#CCCCCC",
    alpha = 0.5
  ) +
  ggplot2::geom_line(
    data = activity_data2,
    ggplot2::aes(
      x = timevar,
      y = var_col,
      color = visualize,
      group = cage_name
    )
  ) +
  ggplot2::scale_color_manual(
    name = "Group",
    values = plotcolors
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom"
  ) +
  ggplot2::coord_cartesian(
    xlim = xminmax_plot,
    ylim = yminmax_plot2
  ) +
  ggplot2::xlab("Time") +
  ggplot2::ylab("movement_mean_per_cage_cm_s_hour") +
  ggplot2::ggtitle(metadata[["study_name"]])

spaghettiplot_example_facet <- spaghettiplot_example +
  ggplot2::facet_wrap(. ~ visualize,
    ncol = 1,
    as.table = FALSE
  )

xlims_example <- as.POSIXct(
  c(
    "2024-01-06 00:00:00",
    "2024-01-08 00:00:00"
  ),
  tz = metadata[["tzone"]]
)
ylims_raw <- range(activity_data |>
  dplyr::filter(timevar >= xlims_example[1] &
    timevar <= xlims_example[2]) |>
  dplyr::pull(var_col), na.rm = TRUE)
ylims_range <- ylims_raw[2] - ylims_raw[1]
ylims_example <- c(
  ylims_raw[1] - 0.05 * ylims_range,
  ylims_raw[2] + 0.05 * ylims_range
)

spaghettiplot_example_xlim <- spaghettiplot_base +
  ggplot2::coord_cartesian(
    xlim = xlims_example,
    ylim = ylims_example
  ) +
  ggplot2::xlab("Time") +
  ggplot2::ylab("movement_mean_per_cage_cm_s_hour") +
  ggplot2::ggtitle(metadata[["study_name"]])

test_that("spaghetti_plot() returns expected value with facet = FALSE", {
  x <- spaghetti_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = FALSE
  )
  expect_true(
    ggplot2::is_ggplot(x)
  )
  expect_identical(
    x$labels,
    spaghettiplot_example$labels
  )
  expect_identical(
    x$theme,
    spaghettiplot_example$theme
  )
  expect_identical(
    x$coordinates,
    spaghettiplot_example$coordinates
  )
  expect_no_error(
    print(x)
  )
})

test_that("spaghetti_plot() returns expected value with facet = TRUE", {
  x2 <- spaghetti_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = TRUE
  )
  expect_true(
    ggplot2::is_ggplot(x2)
  )
  expect_identical(
    x2$labels,
    spaghettiplot_example_facet$labels
  )
  expect_identical(
    x2$theme,
    spaghettiplot_example_facet$theme
  )
  expect_identical(
    x2$coordinates,
    spaghettiplot_example_facet$coordinates
  )
  expect_no_error(
    print(x2)
  )
})

test_that("spaghetti_plot() returns expected value with constrained xlims", {
  x3 <- spaghetti_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    yvar = "movement_mean_per_cage_cm_s_hour",
    xlims = xlims_example,
    facet = FALSE
  )
  expect_true(
    ggplot2::is_ggplot(x3)
  )
  expect_identical(
    x3$labels,
    spaghettiplot_example_xlim$labels
  )
  expect_identical(
    x3$theme,
    spaghettiplot_example_xlim$theme
  )
  expect_identical(
    x3$coordinates,
    spaghettiplot_example_xlim$coordinates
  )
  expect_no_error(
    print(x3)
  )
})

test_that("spaghetti_plot() returns expected value with metadata are not provided and lights-on and lights-off time are", {
  x325 <- spaghetti_plot(
    activity_data = activity_cage_data_example,
    lights_on = "06:00",
    lights_off = "18:00",
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = TRUE
  )
  expect_true(
    ggplot2::is_ggplot(x325)
  )
  expect_true(
    is.null(x325$labels$title)
  )
  expect_identical(
    x325$theme,
    spaghettiplot_example$theme
  )
  expect_identical(
    x325$coordinates,
    spaghettiplot_example$coordinates
  )
  expect_no_error(
    print(x325)
  )
})

test_that("Color warnings are handled as expected", {
  expect_no_warning(
    x3p5 <- spaghetti_plot(
      activity_data = activity_cage_data_example,
      metadata = metadata,
      yvar = "movement_mean_per_cage_cm_s_hour",
      facet = FALSE,
      colors = c("#000000", "#666666")
    )
  )
})

test_that("Color errors are handled as expected", {
  expect_error(
    x4 <- spaghetti_plot(
      activity_data = activity_cage_data_example,
      metadata = metadata,
      yvar = "movement_mean_per_cage_cm_s_hour",
      facet = FALSE,
      colors = c("#000000")
    ),
    "insufficient number of colors provided"
  )
})

test_that("Color warnings are handled as expected", {
  expect_warning(
    x5 <- spaghetti_plot(
      activity_data = activity_cage_data_example,
      metadata = metadata,
      yvar = "movement_mean_per_cage_cm_s_hour",
      facet = FALSE,
      colors = c("#000000", "#333333", "#666666")
    ),
    "more colors provided than number of grouping variables"
  )
})

test_that("Occupancy normalization works as expected with column occupancy_norm as TRUE", {
  activity_cage_data_example2 <- activity_cage_data_example |>
    dplyr::mutate(
      movement_mean_per_cage_cm_s_hour = movement_mean_per_cage_cm_s_hour / animals_cage_quantity,
      occupancy_norm = TRUE
    )
  expect_warning(
    x6 <- spaghetti_plot(
      activity_data = activity_cage_data_example2,
      metadata = metadata,
      yvar = "movement_mean_per_cage_cm_s_hour",
      facet = FALSE
    ),
    "dataset already occupancy normalized"
  )
  expect_true(
    ggplot2::is_ggplot(x6)
  )
  expect_identical(
    x6$labels,
    spaghettiplot_example$labels
  )
  expect_identical(
    x6$theme,
    spaghettiplot_example$theme
  )
  expect_identical(
    x6$coordinates,
    spaghettiplot_example$coordinates
  )
  expect_no_error(
    print(x6)
  )
})

test_that("Plotting works as expected with no occupancy normalization", {
  x7 <- spaghetti_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = FALSE,
    occupancy_norm = FALSE
  )
  expect_true(
    ggplot2::is_ggplot(x7)
  )
  expect_identical(
    x7$labels,
    spaghettiplot_example2$labels
  )
  expect_identical(
    x7$theme,
    spaghettiplot_example2$theme
  )
  expect_identical(
    x7$coordinates,
    spaghettiplot_example2$coordinates
  )
  expect_no_error(
    print(x7)
  )
})

test_that("User-defined x variable works as expected.", {
  activity_cage_data_example3 <- activity_cage_data_example |>
    dplyr::mutate(reltime_weeks_postnatal = reltime(start,
      as.POSIXct("2023-11-01 00:00:00",
        tz = metadata[["tzone"]]
      ),
      units = "weeks"
    ))
  x8 <- spaghetti_plot(
    activity_data = activity_cage_data_example3,
    metadata = metadata,
    yvar = "movement_mean_per_cage_cm_s_hour",
    xvar = "reltime_weeks_postnatal",
    facet = FALSE
  )
  expect_true(
    ggplot2::is_ggplot(x8)
  )
  expect_no_error(
    print(x8)
  )
})

test_that("Function works as expected with null plot title.", {
  metadata2 <- envision_metadata(
    study_name = "",
    tzone = "US/Pacific",
    lights_on = "06:00:00",
    lights_off = "18:00:00",
    study_url = "https://envision.jax.org/org/1001/study/1002/overview"
  )
  x9 <- spaghetti_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata2,
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = FALSE
  )
  expect_true(is.null(x9$labels$title))
})


test_that("Errors thrown when only lights-on or lights-off provided.", {
  expect_error(
    spaghetti_plot(
      activity_data = activity_cage_data_example,
      lights_on = "06:00",
      yvar = "movement_mean_per_cage_cm_s_hour",
      facet = FALSE
    ),
    "metadata is not provided and lights_on or lights_off is not given"
  )
  expect_error(
    spaghetti_plot(
      activity_data = activity_cage_data_example,
      lights_off = "18:00",
      yvar = "movement_mean_per_cage_cm_s_hour",
      facet = FALSE
    ),
    "metadata is not provided and lights_on or lights_off is not given"
  )
})

test_that("spaghetti plot works with custom y axis", {
  x10 <- spaghetti_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    yvar = "movement_mean_per_cage_cm_s_hour",
    ylims = custom_ylim,
    facet = FALSE
  )
  expect_identical(
    x10$coordinates$limits,
    spaghettiplot_example_customy$coordinates$limits
  )
})

test_that("custom x and y axis labels work as expected", {
  x12 <- spaghetti_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    yvar = "movement_mean_per_cage_cm_s_hour",
    x_axis_label = "this is an x axis",
    y_axis_labe = "this is a y axis",
    facet = FALSE
  )
  expect_identical(
    x12$labels$x,
    "this is an x axis"
  )
  expect_identical(
    x12$labels$y,
    "this is a y axis"
  )
})
