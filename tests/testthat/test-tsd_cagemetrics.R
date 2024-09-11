test_that("tsd_cagemetrics() returns a valid tibble with expected column names", {
  expect_no_error(y1 <- tsd_cagemetrics(
    activity_data = activity_cage_data_example,
    var = "movement_mean_per_cage_cm_s_hour",
    occupancy_normalize = TRUE,
    quietly = FALSE
  ))
  expect_true(tibble::is_tibble(y1))
  expect_identical(colnames(y1),
                   c("start", "cage_name", "group_name", "tsd_var",
                     "animals_cage_quantity", "occupancy_norm", "raw", "impute",
                     "circadian", "trend", "residual", "detrended"))
})

test_that("tsd_cagemetrics() returns a warning when imputing as expected", {
  activity_cage_data_example_NA <- activity_cage_data_example
  activity_cage_data_example_NA[48, "movement_mean_per_cage_cm_s_hour"] <- NA
  expect_warning(y2 <- tsd_cagemetrics(
    activity_data = activity_cage_data_example_NA,
    var = "movement_mean_per_cage_cm_s_hour",
    occupancy_normalize = TRUE,
    quietly = FALSE
  ),
  "internal NA values imputed in this dataset"
  )
  expect_true(tibble::is_tibble(y2))
  expect_identical(colnames(y2),
                   c("start", "cage_name", "group_name", "tsd_var",
                     "animals_cage_quantity", "occupancy_norm", "raw", "impute",
                     "circadian", "trend", "residual", "detrended"))
})

test_that("tsd_cagemetrics() works with no occupancy normalization", {
  expect_no_error(y3 <- tsd_cagemetrics(
    activity_data = activity_cage_data_example,
    var = "movement_mean_per_cage_cm_s_hour",
    occupancy_normalize = FALSE
  ))
  expect_true(tibble::is_tibble(y3))
  expect_identical(colnames(y3),
                   c("start", "cage_name", "group_name", "tsd_var",
                     "animals_cage_quantity", "occupancy_norm", "raw", "impute",
                     "circadian", "trend", "residual", "detrended"))
})

