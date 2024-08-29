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
  mutate(
    var_col = movement_mean_per_cage_cm_s_hour / animals_cage_quantity,
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
    ggplot2::is.ggplot(x)
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
  expect_error(
    print(x),
    NA
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
    ggplot2::is.ggplot(x2)
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
  expect_identical(
    x2$facet$params,
    spaghettiplot_example_facet$facet$params
  )
  expect_error(
    print(x2),
    NA
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
    ggplot2::is.ggplot(x3)
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
  expect_error(
    print(x3),
    NA
  )
})

test_that("Color errors are handled as expected", {
  expect_error(
    x4 <- spaghetti_plot(
      activity_data = activity_cage_data_example,
      metadata = metadata,
      yvar = "movement_mean_per_cage_cm_s_hour",
      xlims = xlims_example,
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
      xlims = xlims_example,
      facet = FALSE,
      colors = c("#000000", "#333333", "#666666")
    ),
    "more colors provided than number of grouping variables"
  )
})
