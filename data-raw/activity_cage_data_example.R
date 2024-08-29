# making the dummy dataset called activity_cage_data_example
library("hms")

times <- seq(
  from = as.POSIXct("2024-01-01", tz = "US/Pacific"),
  to = as.POSIXct("2024-01-14 23:00:00", tz = "US/Pacific"),
  by = 3600
)
pattern_g1 <- c(
  5, 4, 3, 3, 4, 4,
  1, 0.5, 0.5, 0.25, 0.25, 0.5,
  0.25, 0.25, 0.25, 0.25, 0.5, 1,
  6, 5.5, 6, 5.5, 5.5, 5
)
pattern_g2 <- c(
  3, 3.5, 3.5, 3, 3.5, 3.5,
  0.25, 0.25, 0.5, 0.25, 0.5, 0.5,
  0.5, 0.25, 0.25, 0.25, 0.5, 1.5,
  4, 4.5, 4.5, 4.5, 4, 3.5
)
n_hours <- length(times)
n_days <- n_hours %/% 24

set.seed(as.numeric(as.POSIXct("2024-01-01 12:25:15")))
activity_cage_data_example <- data.frame(
  start = rep(times, times = 8),
  start_date_local = rep(as.Date(times), times = 8),
  start_time_local = rep(hms::as_hms(times), times = 8),
  study_code = "dummystudy",
  aggregation_seconds = 3600,
  group_name = c(
    rep("G1", times = 4 * n_hours),
    rep("G2", times = 4 * n_hours)
  ),
  cage_name = paste0("C", rep(1:8, each = n_hours)),
  animals_cage_quantity = sample(c(3L, 2L, 1L),
    size = 8 * n_hours,
    replace = TRUE,
    prob = c(0.95, 0.04, 0.01)
  )
) |>
  dplyr::mutate(
    light_cycle = ifelse(as.numeric(start_time_local) < 3600 * 6 |
      as.numeric(start_time_local) >= 3600 * 18,
    "Dark", "Light"
    ),
    movement_mean_per_cage_cm_s_hour = rnorm(
      n = (8 * n_hours),
      mean = 0,
      sd = 0.05
    ),
    movement_mean_per_cage_cm_s_hour = movement_mean_per_cage_cm_s_hour + c(
      rep(pattern_g1, times = n_days * 4),
      rep(pattern_g2, times = n_days * 4)
    ),
    movement_mean_per_cage_cm_s_hour = movement_mean_per_cage_cm_s_hour * animals_cage_quantity,
    wheel_occupancy_mean_per_cage_s_hour = rnorm(
      n = (8 * n_hours),
      mean = 0,
      sd = 0.05
    ),
    wheel_occupancy_mean_per_cage_s_hour = wheel_occupancy_mean_per_cage_s_hour + c(
      rep(pattern_g1, times = n_days * 4),
      rep(pattern_g2, times = n_days * 4)
    ),
    wheel_occupancy_mean_per_cage_s_hour = 0.75 * wheel_occupancy_mean_per_cage_s_hour * animals_cage_quantity,
    food_occupancy_mean_per_cage_s_hour = rnorm(
      n = (8 * n_hours),
      mean = 0,
      sd = 0.05
    ),
    food_occupancy_mean_per_cage_s_hour = food_occupancy_mean_per_cage_s_hour + c(
      rep(pattern_g1, times = n_days * 4),
      rep(pattern_g2, times = n_days * 4)
    ),
    food_occupancy_mean_per_cage_s_hour = 0.5 * food_occupancy_mean_per_cage_s_hour * animals_cage_quantity,
    water_occupancy_mean_per_cage_s_hour = rnorm(
      n = (8 * n_hours),
      mean = 0,
      sd = 0.05
    ),
    water_occupancy_mean_per_cage_s_hour = water_occupancy_mean_per_cage_s_hour + c(
      rep(pattern_g1, times = n_days * 4),
      rep(pattern_g2, times = n_days * 4)
    ),
    water_occupancy_mean_per_cage_s_hour = 0.15 * water_occupancy_mean_per_cage_s_hour * animals_cage_quantity,
    tzone = "US/Pacific"
  ) |>
  tibble::as_tibble()

usethis::use_data(activity_cage_data_example, internal = FALSE, overwrite = TRUE)
