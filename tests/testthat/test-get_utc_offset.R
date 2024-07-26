# Unit tests for the get_utc_offset() function.

test_that("get_utc_offset() returns character for whole number standard time", {
  expect_equal(get_utc_offset(as.POSIXct("2024-01-01 12:00:00",
                                         tz = "US/Pacific"),
                              as_numeric = FALSE), "-08:00")
})

test_that("get_utc_offset() returns numeric for whole number standard time", {
  expect_equal(get_utc_offset(as.POSIXct("2024-01-01 12:00:00",
                                         tz = "US/Pacific"),
                              as_numeric = TRUE), -8)
})

test_that("get_utc_offset() returns character for whole number daylight time", {
  expect_equal(get_utc_offset(as.POSIXct("2024-07-01 12:00:00",
                                         tz = "US/Pacific"),
                              as_numeric = FALSE), "-07:00")
})

test_that("get_utc_offset() returns numeric for whole number daylight time", {
  expect_equal(get_utc_offset(as.POSIXct("2024-07-01 12:00:00",
                                         tz = "US/Pacific"),
                              as_numeric = TRUE), -7)
})

test_that("get_utc_offset() returns character for decimal standard time", {
  expect_equal(get_utc_offset(as.POSIXct("2024-01-01 12:00:00",
                                         tz = "Canada/Newfoundland"),
                              as_numeric = FALSE), "-03:30")
})

test_that("get_utc_offset() returns numeric for decimal standard time", {
  expect_equal(get_utc_offset(as.POSIXct("2024-01-01 12:00:00",
                                         tz = "Canada/Newfoundland"),
                              as_numeric = TRUE), -3.5)
})

test_that("get_utc_offset() returns character for decimal daylight time", {
  expect_equal(get_utc_offset(as.POSIXct("2024-07-01 12:00:00",
                                         tz = "Canada/Newfoundland"),
                              as_numeric = FALSE), "-02:30")
})

test_that("get_utc_offset() returns numeric for decimal daylight time", {
  expect_equal(get_utc_offset(as.POSIXct("2024-07-01 12:00:00",
                                         tz = "Canada/Newfoundland"),
                              as_numeric = TRUE), -2.5)
})

test_that("get_utc_offset() returns character for a positive offset decimal time", {
  expect_equal(get_utc_offset(as.POSIXct("2024-01-01 12:00:00",
                                         tz = "Asia/Kolkata"),
                              as_numeric = FALSE), "+05:30")
})

test_that("get_utc_offset() returns numeric for a positive offset decimal time", {
  expect_equal(get_utc_offset(as.POSIXct("2024-01-01 12:00:00",
                                         tz = "Asia/Kolkata"),
                              as_numeric = TRUE), 5.5)
})

test_that("get_utc_offset() returns character for a positive offset whole time", {
  expect_equal(get_utc_offset(as.POSIXct("2024-01-01 12:00:00",
                                         tz = "Asia/Almaty"),
                              as_numeric = FALSE), "+06:00")
})

test_that("get_utc_offset() returns numeric for a positive offset whoe time", {
  expect_equal(get_utc_offset(as.POSIXct("2024-01-01 12:00:00",
                                         tz = "Asia/Almaty"),
                              as_numeric = TRUE), 6)
})

test_that("get_utc_offset() works for a UTC time stamp passed to the function.", {
  expect_equal(get_utc_offset(ts = as.POSIXct("2024-01-01 12:00:00",
                                              tz = "US/Pacific"),
                              ts_utc = as.POSIXct("2024-01-01 20:00:00 UTC",
                                                  tz = "UTC"),
                              as_numeric = TRUE), -8)
})

utc_datetimes = as.POSIXct(paste0("2024-01-20 ",
                               formatC(0:23, width=2, flag="0"),
                               ":00:00"), tz = "UTC")
pacific_datetimes = utc_datetimes
attributes(pacific_datetimes)$tzone = "US/Pacific"

test_that("get_utc_offset() works for a 24 hours of UTC time stamps passed to the function.", {
  expect_equal(get_utc_offset(ts = pacific_datetimes,
                              ts_utc = pacific_datetimes,
                              as_numeric = TRUE),
               rep(-8, times = 24))
})
