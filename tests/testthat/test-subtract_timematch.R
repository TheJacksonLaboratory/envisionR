test_that("subtract_timematch() returns a valid tibble with expected column names", {
  expect_no_error(y1 <- subtract_timematch(
    activity_data = activity_cage_data_example,
    var = "movement_mean_per_cage_cm_s_hour",
    occupancy_normalize = TRUE,
    quietly = FALSE
  ))
  expect_true(tibble::is_tibble(y1))
  expect_identical(colnames(y1),
                   c("start", "cage_name", "group_name", "subtract_var",
                     "animals_cage_quantity", "occupancy_norm", "raw", "hour",
                     "leading_trailing_na", "internal_na", "impute",
                     "time_matched_subtracted"))
})

test_that("subtract_timematch() works with no occupancy normalization", {
  expect_no_error(y3 <- subtract_timematch(
    activity_data = activity_cage_data_example,
    var = "movement_mean_per_cage_cm_s_hour",
    occupancy_normalize = FALSE
  ))
  expect_true(tibble::is_tibble(y3))
  expect_identical(colnames(y3),
                   c("start", "cage_name", "group_name", "subtract_var",
                     "animals_cage_quantity", "occupancy_norm", "raw", "hour",
                     "leading_trailing_na", "internal_na", "impute",
                     "time_matched_subtracted"))
})

