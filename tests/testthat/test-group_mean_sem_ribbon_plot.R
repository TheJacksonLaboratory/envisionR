# unit tests for mean sem ribbon plot

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

test_that("group_mean_sem_ribbon_plot() returns expected value with facet = FALSE", {
  expect_no_error(y1 <- group_mean_sem_ribbon_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = FALSE
  ))
  expect_true(
    ggplot2::is.ggplot(y1)
  )
  expect_no_error(
    print(y1)
  )
})

test_that("group_mean_sem_ribbon_plot() returns expected value with facet = TRUE", {
  expect_no_error(y2 <- group_mean_sem_ribbon_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = TRUE
  ))
  expect_true(
    ggplot2::is.ggplot(y2)
  )
  expect_no_error(
    print(y2)
  )
})

test_that("group_mean_sem_ribbon_plot() works with an alternative timevar", {
  activity_cage_data_example_alttime <- activity_cage_data_example |>
    dplyr::mutate(alttime = start)
  expect_no_error(y3 <- group_mean_sem_ribbon_plot(
    activity_data = activity_cage_data_example_alttime,
    metadata = metadata,
    xvar = "alttime",
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = TRUE
  ))
  expect_true(
    ggplot2::is.ggplot(y3)
  )
  expect_no_error(
    print(y3)
  )
})

test_that("group_mean_sem_ribbon_plot() throws error for insufficient colors", {
  expect_error(y4 <- group_mean_sem_ribbon_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = TRUE,
    colors = c("#CCCCCC"),
  "insufficient number of colors provided"),
  )
})

test_that("group_mean_sem_ribbon_plot() throws warning for too many colors", {
  expect_warning(y5 <- group_mean_sem_ribbon_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = TRUE,
    colors = c("#CCCCCC","#DDDDDD","#EEEEEE")),
    "more colors provided than number of grouping variables"
  )
  expect_true(
    ggplot2::is.ggplot(y5)
  )
  expect_no_error(
    print(y5)
  )
})

test_that("group_mean_sem_ribbon_plot() works warning for too many colors", {
  expect_no_error(y6 <- group_mean_sem_ribbon_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = TRUE,
    colors = c("#CCCCCC","#DDDDDD")
    )
  )
  expect_true(
    ggplot2::is.ggplot(y6)
  )
  expect_no_error(
    print(y6)
  )
})

test_that("group_mean_sem_ribbon_plot() works with no plot title", {
  metadata_noplottitle <- envision_metadata(
    study_name = "",
    tzone = "US/Pacific",
    lights_on = "06:00:00",
    lights_off = "18:00:00",
    study_url = "https://envision.jax.org/org/1001/study/1002/overview"
  )
  expect_no_error(y7 <- group_mean_sem_ribbon_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata_noplottitle,
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = FALSE
  ))
  expect_true(
    ggplot2::is.ggplot(y7)
  )
  expect_true(
    is.null(y7$labels$title)
  )
  expect_no_error(
    print(y7)
  )
})

test_that("group_mean_sem_ribbon_plot() throws error for no lights-on and lights-off info", {
  expect_error(y8 <- group_mean_sem_ribbon_plot(
    activity_data = activity_cage_data_example,
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = TRUE,
    lightdark = TRUE),
    "metadata is not provided and lights_on or lights_off is not given")
})

test_that("group_mean_sem_ribbon_plot() works with lights-on and lights-off given as argument", {
  expect_no_error(y8 <- group_mean_sem_ribbon_plot(
    activity_data = activity_cage_data_example,
    lights_on = "06:00",
    lights_off = "18:00",
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = FALSE
  ))
  expect_true(
    ggplot2::is.ggplot(y8)
  )
  expect_no_error(
    print(y8)
  )
})

test_that("group_mean_sem_ribbon_plot() works with xlims alterred", {
  y9_xlim <- as.POSIXct(c("2024-01-04 12:00:00",
                         "2024-01-08 12:00:00"),
                       tz = metadata$tzone)
  expect_no_error(y9 <- group_mean_sem_ribbon_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = FALSE,
    xlim = y9_xlim
  ))
  expect_true(
    ggplot2::is.ggplot(y9)
  )
  expect_identical(
    y9$coordinates$limits$x,
    y9_xlim
  )
  expect_no_error(
    print(y9)
  )
})

test_that("group_mean_sem_ribbon_plot() works with xlims and ylims alterred", {
  y10_xlim <- as.POSIXct(c("2024-01-04 12:00:00",
                           "2024-01-08 12:00:00"),
                         tz = metadata$tzone)
  y10_ylim <- c(2,4)
  expect_no_error(y10 <- group_mean_sem_ribbon_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = FALSE,
    xlim = y10_xlim,
    ylim = y10_ylim
  ))
  expect_true(
    ggplot2::is.ggplot(y10)
  )
  expect_identical(
    y10$coordinates$limits$x,
    y10_xlim
  )
  expect_identical(
    y10$coordinates$limits$y,
    y10_ylim
  )
  expect_no_error(
    print(y10)
  )
})

test_that("group_mean_sem_ribbon_plot() returns expected values with x and y axis labels specified", {
  expect_no_error(y11 <- group_mean_sem_ribbon_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    yvar = "movement_mean_per_cage_cm_s_hour",
    facet = FALSE,
    y_axis_label = "y label",
    x_axis_label = "x label"
  ))
  expect_true(
    ggplot2::is.ggplot(y11)
  )
  expect_equal(
    y11$labels$x,
    "x label"
  )
  expect_equal(
    y11$labels$y,
    "y label"
  )
  expect_no_error(
    print(y11)
  )
})
