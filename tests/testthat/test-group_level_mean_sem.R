# unit tests for the group_level_mean_sem() function

# setup
activity_norm_meansem <- activity_cage_data_example |>
  dplyr::group_by(group_name, start) |>
  dplyr::mutate(norm_activity = movement_mean_per_cage_cm_s_hour / animals_cage_quantity) |>
  dplyr::summarize(
    mean_activity = weighted.mean(norm_activity, na.rm = TRUE, w = animals_cage_quantity),
    sem_activity = weighted_sem(norm_activity, w = animals_cage_quantity, na.rm = TRUE),
    ll_activity = mean_activity - sem_activity,
    ul_activity = mean_activity + sem_activity
  ) |>
  tibble::as_tibble()

activity_norm_nonweighted_meansem <- activity_cage_data_example |>
  dplyr::group_by(group_name, start) |>
  dplyr::mutate(norm_activity = movement_mean_per_cage_cm_s_hour / animals_cage_quantity) |>
  dplyr::summarize(
    mean_activity = mean(norm_activity, na.rm = TRUE, w = animals_cage_quantity),
    sem_activity = sd(norm_activity, na.rm = TRUE) / sqrt(length(which(!is.na(norm_activity)))),
    ll_activity = mean_activity - sem_activity,
    ul_activity = mean_activity + sem_activity
  ) |>
  tibble::as_tibble()

activity_nonorm_meansem <- activity_cage_data_example |>
  group_by(group_name, start) |>
  summarize(
    mean_activity = weighted.mean(movement_mean_per_cage_cm_s_hour, na.rm = TRUE, w = animals_cage_quantity),
    sem_activity = weighted_sem(movement_mean_per_cage_cm_s_hour, w = animals_cage_quantity, na.rm = TRUE),
    ll_activity = mean_activity - sem_activity,
    ul_activity = mean_activity + sem_activity
  ) |>
  tibble::as_tibble()

test_that("group_level_mean_sem() function works as expected with default parameters", {
  expect_identical(
    group_level_mean_sem(activity_cage_data_example),
    activity_norm_meansem
  )
})

test_that("group_level_mean_sem() function works as expected with no occupancy normalization", {
  expect_identical(
    group_level_mean_sem(activity_cage_data_example,
      occupancy_normalize = FALSE
    ),
    activity_nonorm_meansem
  )
})

test_that("group_level_mean_sem() function works as expected with no occupancy normalization", {
  activity_cage_data_example2 <- activity_cage_data_example |>
    mutate(
      movement_mean_per_cage_cm_s_hour = movement_mean_per_cage_cm_s_hour / animals_cage_quantity,
      occupancy_norm = TRUE
    )
  expect_warning(
    y1 <- group_level_mean_sem(activity_cage_data_example2,
      occupancy_normalize = TRUE
    ),
    "^dataset already occupancy normalized"
  )
  expect_identical(y1, activity_norm_meansem)
})

test_that("group_level_mean_sem() function works as expected with no occupancy weighting", {
  expect_identical(
    group_level_mean_sem(activity_cage_data_example,
      occupancy_normalize = TRUE,
      occupancy_weighted = FALSE
    ),
    activity_norm_nonweighted_meansem
  )
})
