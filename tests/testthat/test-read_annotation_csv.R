# Unit tests for the read_annotation_csv() function.

# Getting required libraries
require("tibble")
require("janitor")
require("dplyr")
require("hms")

# Writing out a dummy CSV file
tempcsv_1 <- tempfile("testannotation", fileext = ".csv")
writeLines(annotation_csv_example, tempcsv_1)

# Writing out the CSV file that should throw an unambiguous time code error
csv_lines_error <- gsub("19:17:03", "18:17:03", annotation_csv_example)
tempcsv_2 <- tempfile("testannotation_error", fileext = ".csv")
writeLines(csv_lines_error, tempcsv_2)

# Writing out the CSV file that should work with the test version.
csv_lines_testversion <- c("# Version: TEST", annotation_csv_example)
tempcsv_3 <- tempfile("testannotation_version", fileext = ".csv")
writeLines(csv_lines_testversion, tempcsv_3)

# Writing out the CSV file that should throw a version doesn't exist error.
csv_lines_versionerror <- c("# Version: DOESNOTEXIST", annotation_csv_example)
tempcsv_4 <- tempfile("testannotation_version_error", fileext = ".csv")
writeLines(csv_lines_versionerror, tempcsv_4)

# Making the data frame with variable types that should match the wrangled CSV file
csv_out <- read.csv(text = annotation_csv_example) |>
  tibble::as_tibble() |>
  janitor::clean_names() |>
  dplyr::mutate(
    id = as.numeric(paste0(as.character(id), ".0")),
    created = as.POSIXct(as.POSIXct(created, tz = "UTC", optional = TRUE),
      tz = "US/Pacific", optional = TRUE
    ),
    created_date_local = as.Date(created_date_local),
    created_time_local = hms::as_hms(created_time_local),
    pin_start_date_local = as.Date(pin_start_date_local),
    pin_start_time_local = hms::as_hms(pin_start_time_local),
    pin_end_date_local = as.Date(pin_end_date_local),
    pin_end_time_local = hms::as_hms(pin_end_time_local),
    study_code = as.character(study_code),
    group_name = as.character(group_name),
    cage_name = as.character(cage_name),
    creator = as.character(creator),
    contents = as.character(contents),
    reply_to = as.character(reply_to),
    hashtags = as.character(hashtags),
    pin_start_time = as.POSIXct(
      paste(
        as.character(pin_start_date_local),
        as.character(pin_start_time_local)
      ),
      tz = "US/Pacific", optional = TRUE
    ),
    pin_end_time = as.POSIXct(
      paste(
        as.character(pin_end_date_local),
        as.character(pin_end_time_local)
      ),
      tz = "US/Pacific",
      optional = TRUE
    ),
    tzone = "US/Pacific"
  ) |>
  dplyr::mutate(created_time_local = hms::as_hms(round(created_time_local, 0)))

# Doing the tests
test_that("read_annotation_csv() returns expected tibble with default parameters", {
  expect_equal(
    read_annotation_csv(tempcsv_1, tzone = "US/Pacific"),
    csv_out
  )
})

test_that("read_annotation_csv() returns data in correct time zone", {
  expect_equal(
    read_annotation_csv(tempcsv_1, tzone = "US/Pacific") |>
      mutate(
        created = as.character(created),
        pin_start_time = as.character(pin_start_time),
        pin_end_time = as.character(pin_end_time)
      ),
    csv_out |>
      mutate(
        created = as.character(created),
        pin_start_time = as.character(pin_start_time),
        pin_end_time = as.character(pin_end_time)
      )
  )
})

test_that("read_annotation_csv() returns no warnings with default parameters and matching time zones", {
  expect_no_warning(read_annotation_csv(tempcsv_1, tzone = "US/Pacific"))
})

test_that("read_annotation_csv() throws a warning when time zone is omitted", {
  expect_warning(read_annotation_csv(tempcsv_1),
    regexp = "^Assuming time zone: \\w+/\\w+\\. Set time zone explicitly if different.$"
  )
})

test_that("read_annotation_csv() throws a warning when started time zone mismatches UTC offset", {
  expect_warning(read_annotation_csv(tempcsv_1, tzone = "Canada/Newfoundland"),
    regexp = "^UTC offset for the \\w+/\\w+ time zone mismatches suggested time zones.$"
  )
})

test_that("read_annotation_csv() throws an error when started time zone does not exist", {
  expect_error(read_annotation_csv(tempcsv_1, tzone = "Canada/New_Foundland"),
    regexp = "^time zone \\w+/\\w+ is not a system time zone.$"
  )
})

test_that("read_annotation_csv() throws an error when multiple irreconcilable UTC offsets are present", {
  expect_error(read_annotation_csv(tempcsv_2),
    regexp = "^could not assume a time zone unambiguously.$"
  )
})

test_that("read_annotation_csv() works with a different version", {
  expect_equal(
    read_annotation_csv(tempcsv_3, tzone = "US/Pacific"),
    csv_out
  )
})

test_that("read_annotation_csv() throws an error when a version doesn't exist", {
  expect_error(read_annotation_csv(tempcsv_4, tzone = "US/Pacific"),
    regexp = "^invalid Envision csv version number: vDOESNOTEXIST$"
  )
})

# Removing temp files
file.remove(tempcsv_1)
file.remove(tempcsv_2)
file.remove(tempcsv_3)
file.remove(tempcsv_4)
