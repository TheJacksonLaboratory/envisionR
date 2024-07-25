# Unit tests for the experiment_time_encoding() function.

require(lubridate)

test_rawtimes = ymd_hms(paste0("2024-04-01 0",1:9, ":00:00"), tz = "UTC")
test_reftime  = ymd_hms("2024-01-01 00:00:00", tz = "UTC")
test_reftimes = rep(test_reftime, times = length(test_rawtimes))

# test with only one reference time
test_that("a single reference time works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes[1],
                                        units = "seconds"),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800))
})

# test with two reference times and unequal raw time length
test_that("an unequal amount of raw and reference times throws an error when there are 2+ reftimes", {
  expect_error(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes[1:2],
                                        units = "seconds"),
               "rawtimes and reftimes are different lengths")
})

# seconds
test_that("seconds with no offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                           reftimes = test_reftimes,
                           units = "seconds"),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800))
})

test_that("seconds with positive offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "seconds",
                                        offset = 10),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) + 10)
})

test_that("seconds with negative offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "seconds",
                                        offset = -10),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) - 10)
})

# minutes
test_that("minutes with no offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "minutes"),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / 60)
})

test_that("minutes with positive offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "minutes",
                                        offset = 10),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / 60 + 10)
})

test_that("minutes with negative offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "minutes",
                                        offset = -10),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                  7891200, 7894800) / 60 - 10)
})

# hours
test_that("hours with no offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "hours"),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / (60 * 60))
})

test_that("hours with positive offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "hours",
                                        offset = 10),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / (60 * 60) + 10)
})

test_that("hours with negative offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "hours",
                                        offset = -10),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / (60 * 60) - 10)
})

# days
test_that("days with no offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "days"),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / (24 * 60 * 60))
})

test_that("days with positive offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "days",
                                        offset = 10),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / (24 * 60 * 60) + 10)
})

test_that("days with negative offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "days",
                                        offset = -10),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / (24 * 60 * 60) - 10)
})

# weeks
test_that("weeks with no offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "weeks"),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / (7 * 24 * 60 * 60))
})

test_that("weeks with positive offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "weeks",
                                        offset = 10),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / (7 * 24 * 60 * 60) + 10)
})

test_that("weeks with negative offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "weeks",
                                        offset = -10),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / (7 * 24 * 60 * 60) - 10)
})

# months
test_that("months with no offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "months"),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / ((365.25 * 24 * 60 * 60) / 12))
})

test_that("months with positive offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "months",
                                        offset = 10),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / ((365.25 * 24 * 60 * 60) / 12) + 10)
})

test_that("months with negative offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "months",
                                        offset = -10),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / ((365.25 * 24 * 60 * 60) / 12) - 10)
})

# years
test_that("years with no offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "years"),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / (365.25 * 24 * 60 * 60))
})

test_that("years with positive offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "years",
                                        offset = 10),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / (365.25 * 24 * 60 * 60) + 10)
})

test_that("years with negative offset works", {
  expect_equal(experiment_time_encoding(rawtimes = test_rawtimes,
                                        reftimes = test_reftimes,
                                        units = "years",
                                        offset = -10),
               c(7866000, 7869600, 7873200, 7876800, 7880400, 7884000, 7887600,
                 7891200, 7894800) / (365.25 * 24 * 60 * 60) - 10)
})
