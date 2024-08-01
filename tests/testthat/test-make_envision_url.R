# Unit tests for the make_envision_url() function.

url_return <- "https://envision.jax.org/org/1001/study/1001/cage/1001?metricsTab=cage&rangeEnd=1686584700000&rangeStart=1686563100000&videoStart=1686573900000&videoStream=overlay"

test_that("test that make_envision_url() returns a well-formed Envision URL", {
  expect_equal(
    make_envision_url(
      org = 1001,
      study = 1001,
      cage = 1001,
      vidstart = as.POSIXct("2023-06-12 07:45:00", tz = "US/Central"),
      windowstart_h = 3,
      windowend_h = 3,
      metricstab = "cage",
      videostream = "overlay"
    ),
    url_return
  )
})

test_that("test that make_envision_url() can pull out slashes at the end of the URL base", {
  expect_equal(
    make_envision_url(
      org = 1001,
      study = 1001,
      cage = 1001,
      vidstart = as.POSIXct("2023-06-12 07:45:00", tz = "US/Central"),
      windowstart_h = 3,
      windowend_h = 3,
      metricstab = "cage",
      videostream = "overlay"
    ),
    url_return
  )
})
