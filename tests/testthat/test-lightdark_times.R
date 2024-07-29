# unit tests for the function lightdark_times()

require("tibble")

test_that("works with lights-off AFTER lights-on", {
  expect_equal(lightdark_times(lights_on_time = "06:00",
                               lights_off_time = "18:00",
                               start_date = "2024-06-02",
                               end_date = "2024-06-03",
                               tzone="US/Pacific"),
               data.frame(lights_off = as.POSIXct(c("2024-06-01 18:00:00",
                                                 "2024-06-02 18:00:00",
                                                 "2024-06-03 18:00:00"),
                                               tz = "US/Pacific"),
                          lights_on = as.POSIXct(c("2024-06-02 06:00:00",
                                                "2024-06-03 06:00:00",
                                                "2024-06-04 06:00:00"),
                                              tz = "US/Pacific")) |>
                 tibble::as_tibble())
})

test_that("works with lights-off BEFORE lights-on", {
  expect_equal(lightdark_times(lights_on_time = "18:00",
                               lights_off_time = "06:00",
                               start_date = "2024-06-02",
                               end_date = "2024-06-03",
                               tzone="US/Pacific"),
               data.frame(lights_off = as.POSIXct(c("2024-06-02 06:00:00",
                                                    "2024-06-03 06:00:00"),
                                                  tz = "US/Pacific"),
                          lights_on = as.POSIXct(c("2024-06-02 18:00:00",
                                                   "2024-06-03 18:00:00"),
                                                 tz = "US/Pacific")) |>
                 tibble::as_tibble())
})

test_that("throws error for improperly formatted start date", {
  expect_error(lightdark_times(lights_on_time = "18:00",
                               lights_off_time = "06:00",
                               start_date = "2024-06-023",
                               end_date = "2024-06-03",
                               tzone="US/Pacific"),
               "start date and end date should be formatted as YYYY-MM-DD.")
})

test_that("throws error for same lights-on and lights-off time", {
  expect_error(lightdark_times(lights_on_time = "18:00",
                               lights_off_time = "18:00",
                               start_date = "2024-06-02",
                               end_date = "2024-06-03",
                               tzone="US/Pacific"),
               "lights-on and lights-off must be different times.")
})


test_that("throws error for improperly formatted end date", {
  expect_error(lightdark_times(lights_on_time = "18:00",
                               lights_off_time = "06:00",
                               start_date = "2024-06-02",
                               end_date = "2024-06-033",
                               tzone="US/Pacific"),
               "start date and end date should be formatted as YYYY-MM-DD.")
})

test_that("throws error for improperly formatted lights-on time", {
  expect_error(lightdark_times(lights_on_time = "18:000",
                               lights_off_time = "06:00",
                               start_date = "2024-06-02",
                               end_date = "2024-06-03",
                               tzone="US/Pacific"),
               "lights-on and lights-off times should be formatted as HH:MM and be in 24 hour time.")
})

test_that("throws error for improperly formatted lights-off time", {
  expect_error(lightdark_times(lights_on_time = "18:00",
                               lights_off_time = "06:000",
                               start_date = "2024-06-02",
                               end_date = "2024-06-03",
                               tzone="US/Pacific"),
               "lights-on and lights-off times should be formatted as HH:MM and be in 24 hour time.")
})
