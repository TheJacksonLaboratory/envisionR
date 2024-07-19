# Unit tests for the get_utc_offset() function.

test_that("get_utc_offset() returns character for whole number standard time", {
  expect_equal(get_utc_offset(ymd_hms("2024-01-01 12:00:00",
                                      tz = "US/Pacific"),
                              as_numeric = FALSE), "-08:00")
})

test_that("get_utc_offset() returns numeric for whole number standard time", {
  expect_equal(get_utc_offset(ymd_hms("2024-01-01 12:00:00",
                                      tz = "US/Pacific"),
                              as_numeric = TRUE), -8)
})

test_that("get_utc_offset() returns character for whole number daylight time", {
  expect_equal(get_utc_offset(ymd_hms("2024-07-01 12:00:00",
                                      tz = "US/Pacific"),
                              as_numeric = FALSE), "-07:00")
})

test_that("get_utc_offset() returns numeric for whole number daylight time", {
  expect_equal(get_utc_offset(ymd_hms("2024-07-01 12:00:00",
                                      tz = "US/Pacific"),
                              as_numeric = TRUE), -7)
})



test_that("get_utc_offset() returns character for decimal standard time", {
  expect_equal(get_utc_offset(ymd_hms("2024-01-01 12:00:00",
                                      tz = "Canada/Newfoundland"),
                              as_numeric = FALSE), "-03:30")
})

test_that("get_utc_offset() returns numeric for decimal standard time", {
  expect_equal(get_utc_offset(ymd_hms("2024-01-01 12:00:00",
                                      tz = "Canada/Newfoundland"),
                              as_numeric = TRUE), -3.5)
})

test_that("get_utc_offset() returns character for decimal daylight time", {
  expect_equal(get_utc_offset(ymd_hms("2024-07-01 12:00:00",
                                      tz = "Canada/Newfoundland"),
                              as_numeric = FALSE), "-02:30")
})

test_that("get_utc_offset() returns numeric for decimal daylight time", {
  expect_equal(get_utc_offset(ymd_hms("2024-07-01 12:00:00",
                                      tz = "Canada/Newfoundland"),
                              as_numeric = TRUE), -2.5)
})

