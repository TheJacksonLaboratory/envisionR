# Unit tests for envision_metadata.R
require("hms")
require("dplyr")

offset_here <- get_utc_offset(Sys.time(), as_numeric = TRUE)
dst_here <- as.POSIXlt(Sys.time())$isdst == 1
tz_here <- timezones_df |>
  filter(utc_offset_h == offset_here &
    is_dst == dst_here &
    assume == 1) |>
  dplyr::pull(tz_name)

url <- "https://envision.jax.org/org/1001/study/1002/overview"
envision_meta_1 <- list(
  study_name = "A", tzone = tz_here,
  lights_on = hms::as_hms("06:00:00"),
  lights_off = hms::as_hms("18:00:00"),
  org = 1001, study = 1002
)

envision_meta_2 <- list(
  study_name = "A", tzone = tz_here,
  lights_on = hms::hms(NA),
  lights_off = hms::hms(NA),
  org = 1001, study = 1002
)

envision_meta_3 <- list(
  study_name = "A", tzone = NA,
  lights_on = hms::as_hms("06:00:00"),
  lights_off = hms::as_hms("18:00:00"),
  org = 1001, study = 1002
)

envision_meta_4 <- list(
  study_name = "A", tzone = tz_here,
  lights_on = hms::as_hms("06:00:00"),
  lights_off = hms::as_hms("18:00:00"),
  org = as.numeric(NA), study = as.numeric(NA)
)

test_that("envision_metadata() works as expected with all input", {
  expect_equal(
    envision_metadata(
      study_name = "A",
      tzone = tz_here,
      lights_on = "06:00:00",
      lights_off = "18:00:00",
      study_url = url
    ),
    envision_meta_1
  )
})

test_that("envision_metadata() works as expected with tzone missing", {
  expect_warning(
    x1 <- envision_metadata(
      study_name = "A",
      lights_on = "06:00:00",
      lights_off = "18:00:00",
      study_url = url
    ),
    "no time zone information, assuming the time zone:"
  )
  expect_equal(x1, envision_meta_1)
})

test_that("envision_metadata() works as expected with tzone missing and quietly", {
  expect_no_warning(x2 <- envision_metadata(
    study_name = "A",
    lights_on = "06:00:00",
    lights_off = "18:00:00",
    study_url = url,
    quietly = TRUE
  ))
  expect_equal(x2, envision_meta_1)
})

test_that("envision_metadata() works as expected with missing lights-on and lights-off input", {
  expect_warning(
    x3 <- envision_metadata(
      study_name = "A",
      tzone = tz_here,
      study_url = url
    ),
    regexp = "no values given for lights_on or lights_off"
  )
  expect_equal(x3, envision_meta_2)
})

test_that("envision_metadata() works as expected with missing lights-on and lights-off input and quietly TRUE", {
  expect_no_warning(x4 <- envision_metadata(
    study_name = "A",
    tzone = tz_here,
    study_url = url,
    quietly = TRUE
  ))
  expect_equal(x4, envision_meta_2)
})

test_that("envision_metadata() works as expected with tzone missing and override_tzassume", {
  expect_warning(
    x5 <- envision_metadata(
      study_name = "A",
      lights_on = "06:00:00",
      lights_off = "18:00:00",
      study_url = url,
      override_tzassume = TRUE
    ),
    "no time zone information, using NA"
  )
  expect_equal(x5, envision_meta_3)
})

test_that("envision_metadata() works as expected with tzone missing and override_tzassume and quietly TRUE", {
  expect_no_warning(x6 <- envision_metadata(
    study_name = "A",
    lights_on = "06:00:00",
    lights_off = "18:00:00",
    study_url = url,
    override_tzassume = TRUE,
    quietly = TRUE
  ))
  expect_equal(x6, envision_meta_3)
})

test_that("envision_metadata() returns NA if study_url left NULL", {
  expect_equal(
    envision_metadata(
      study_name = "A",
      tzone = tz_here,
      lights_on = "06:00:00",
      lights_off = "18:00:00"
    ),
    envision_meta_4
  )
})

# Unit tests for the tzone block
test_that("envision_metadata() throws an error for an improperly formatted time zone", {
  expect_error(
    envision_metadata(
      study_name = "A",
      tzone = "US/Paciffic",
      lights_on = "06:00:00",
      lights_off = "18:00:00",
      study_url = url
    ),
    "time zone US/Paciffic is not a valid time zone name"
  )
})

# Unit tests for the lights-on and lights-off block
test_that("envision_metadata() throws an error with improperly formatted lights-on", {
  expect_error(
    envision_metadata(
      study_name = "A",
      tzone = tz_here,
      lights_on = "06:00",
      lights_off = "18:00:00",
      study_url = url
    ),
    "reformat lights_on and/or lights_off as %HH:%MM:%SS \\(you entered \\d{1,2}:\\d{1,2}[:]?\\d{0,2} for lights_on and \\d{1,2}:\\d{1,2}[:]?\\d{0,2} for lights_off\\)"
  )
})

test_that("envision_metadata() throws an error with improperly formatted lights-off", {
  expect_error(
    envision_metadata(
      study_name = "A",
      tzone = tz_here,
      lights_on = "06:00:00",
      lights_off = "18:00",
      study_url = url
    ),
    "reformat lights_on and/or lights_off as %HH:%MM:%SS \\(you entered \\d{1,2}:\\d{1,2}[:]?\\d{0,2} for lights_on and \\d{1,2}:\\d{1,2}[:]?\\d{0,2} for lights_off\\)"
  )
})

test_that("envision_metadata() throws an error when lights-on and lights-off are separated by less than 8 hours", {
  expect_error(
    envision_metadata(
      study_name = "A",
      tzone = tz_here,
      lights_on = "06:00:00",
      lights_off = "13:00:00",
      study_url = url
    ),
    "lights-on and lights-off separated by less than 8 hours, check input"
  )
})

test_that("envision_metadata() throws a warning when lights-on and lights-off are separated by less than 8 hours and force_lightcycle is true", {
  expect_warning(
    envision_metadata(
      study_name = "A",
      tzone = tz_here,
      lights_on = "06:00:00",
      lights_off = "13:00:00",
      study_url = url,
      force_lightcycle = TRUE
    ),
    "lights-on and lights-off separated by less than 8 hours, check input"
  )
})

test_that("envision_metadata() throws no warning when lights-on and lights-off are separated by less than 8 hours and force_lightcycle and quietly are both true", {
  expect_no_warning(envision_metadata(
    study_name = "A",
    tzone = tz_here,
    lights_on = "06:00:00",
    lights_off = "13:00:00",
    study_url = url,
    force_lightcycle = TRUE,
    quietly = TRUE
  ))
})

test_that("envision_metadata() throws an error when only lights-on is given", {
  expect_error(
    envision_metadata(
      study_name = "A",
      tzone = tz_here,
      lights_on = "06:00:00",
      study_url = url
    ),
    "only one value for lights_on and lights_off provided"
  )
})


test_that("envision_metadata() throws an error when only lights-off is given", {
  expect_error(
    envision_metadata(
      study_name = "A",
      tzone = tz_here,
      lights_off = "18:00:00",
      study_url = url
    ),
    "only one value for lights_on and lights_off provided"
  )
})

# Unit tests for the URL block
test_that("envision_metadata() throws an error a bad URL is given", {
  expect_error(
    envision_metadata(
      study_name = "A",
      tzone = tz_here,
      lights_on = "06:00:00",
      lights_off = "18:00:00",
      study_url = "envision.jax.org"
    ),
    "study_url is not properly formed, ensure that contains has envision.jax.org/org/000/study/0000 where 0s are replace by digits"
  )
})
