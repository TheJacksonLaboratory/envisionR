# unit tests for light-dark violin plots

# set up for testing
metadata <- envision_metadata(
  study_name = "A",
  tzone = "US/Pacific",
  lights_on = "06:00:00",
  lights_off = "18:00:00",
  study_url = "https://envision.jax.org/org/1001/study/1002/overview"
)

metadata_blankstudy <- envision_metadata(
  study_name = "",
  tzone = "US/Pacific",
  lights_on = "06:00:00",
  lights_off = "18:00:00",
  study_url = "https://envision.jax.org/org/1001/study/1002/overview"
)

test_that("lightdark_violin_plot() returns a valid ggplot2 object with group", {
  expect_no_error(y1 <- lightdark_violin_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    visualize_on = "group_name",
    yvar = "movement_mean_per_cage_cm_s_hour"
  ))
  expect_no_error(print(y1))
})

test_that("lightdark_violin_plot() returns a valid ggplot2 object with minoccupancy", {
  expect_no_error(y2 <- lightdark_violin_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    visualize_on = "minoccupancy",
    yvar = "movement_mean_per_cage_cm_s_hour"
  ))
  expect_no_error(print(y2))
})

test_that("lightdark_violin_plot() throws an error for insufficient plot colors", {
  expect_error(
    y3 <- lightdark_violin_plot(
      activity_data = activity_cage_data_example,
      metadata = metadata,
      visualize_on = "group_name",
      yvar = "movement_mean_per_cage_cm_s_hour",
      colors = c("#000000")
    ),
    "insufficient number of colors provided"
  )
})

test_that("lightdark_violin_plot() throws a warning for too many plot colors", {
  expect_warning(
    y4 <- lightdark_violin_plot(
      activity_data = activity_cage_data_example,
      metadata = metadata,
      visualize_on = "group_name",
      yvar = "movement_mean_per_cage_cm_s_hour",
      colors = c("#000000", "#111111", "#222222")
    ),
    "more colors provided than number of grouping variables"
  )
  expect_no_error(print(y4))
})

test_that("lightdark_violin_plot() works as expected with user-defined colors", {
  expect_no_error(y5 <- lightdark_violin_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata,
    visualize_on = "group_name",
    yvar = "movement_mean_per_cage_cm_s_hour",
    colors = c("#000000", "#111111")
  ))
  expect_no_error(print(y5))
})

test_that("lightdark_violin_plot() works as expected with no metadata", {
  expect_no_error(y6 <- lightdark_violin_plot(
    activity_data = activity_cage_data_example,
    visualize_on = "group_name",
    yvar = "movement_mean_per_cage_cm_s_hour"
  ))
  expect_no_error(print(y6))
})

test_that("lightdark_violin_plot() works as expected with a blank study ID", {
  expect_no_error(y7 <- lightdark_violin_plot(
    activity_data = activity_cage_data_example,
    metadata = metadata_blankstudy,
    visualize_on = "group_name",
    yvar = "movement_mean_per_cage_cm_s_hour"
  ))
  expect_no_error(print(y7))
  expect_equal(y7$theme$title, NULL)
})
