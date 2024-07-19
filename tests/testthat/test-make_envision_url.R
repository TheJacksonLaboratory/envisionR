# Unit tests for the make_envision_url() function.

url_return = "https://app.murine.net/org//4/study/84/cage/434?metricsTab=cage&rangeEnd=1686584700000&rangeStart=1686563100000&videoStart=1686573900000&videoStream=overlay"

test_that("test that make_envision_url() returns a well-formed Envision URL", {
  expect_equal(make_envision_url(org = 4,
                                 study = 84,
                                 cage = 434,
                                 vidstart = ymd_hms("2023-06-12 07:45:00", tz = "US/Central"),
                                 windowstart_h = 3,
                                 windowend_h = 3,
                                 metricstab = "cage",
                                 videostream = "overlay",
                                 url_base = "https://app.murine.net/org/"),
               url_return)
})
