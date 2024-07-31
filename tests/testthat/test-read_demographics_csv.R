# Unit tests for read_demographics_csv() and its two wrapper functions

# Test set-up

# Getting required libraries
require("tibble")
require("janitor")
require("dplyr")
require("hms")

# Writing out a dummy CSV file
tempcsv_1 = tempfile("testdemographics", fileext = ".csv")
writeLines(demographics_csv_example, tempcsv_1)

# Writing out a dummy CSV file to test versioning
tempcsv_2 = tempfile("testversion", fileext = ".csv")
writeLines(c("# Version: TEST", demographics_csv_example), tempcsv_2)

# Writing out a dummy CSV file to test versioning error
tempcsv_3 = tempfile("testerror", fileext = ".csv")
writeLines(c("# Version: ERROR", demographics_csv_example), tempcsv_3)

csv_demo_out = read.csv(text = demographics_csv_example,
                        na.strings = c("","NA"),
                        colClasses = c(rep("character", times = 18))) |>
  tibble::as_tibble() |>
  janitor::clean_names() |>
  dplyr::mutate(birth_date = as.POSIXct(birth_date, tz = "UTC"),
                death_date = as.POSIXct(death_date, tz = "UTC"))

# Doing the tests
test_that("read_demographics_csv() returns expected tibble with default parameters", {
  expect_equal(read_demographics_csv(tempcsv_1),
               csv_demo_out |>
                 mutate(birth_date = as.Date(birth_date),
                        death_date = as.Date(death_date)))
})

test_that("read_animalscages_csv() wrapper works", {
  expect_equal(read_animalscages_csv(tempcsv_1),
               csv_demo_out |>
                 mutate(birth_date = as.Date(birth_date),
                        death_date = as.Date(death_date)))
})

test_that("read_animalsandcages_csv() wrapper works", {
  expect_equal(read_animalsandcages_csv(tempcsv_1),
               csv_demo_out |>
                 mutate(birth_date = as.Date(birth_date),
                        death_date = as.Date(death_date)))
})


test_that("read_demographics_csv() returns properly formatted data when date_only is set to FALSE", {
  expect_equal(read_demographics_csv(tempcsv_1, date_only = FALSE),
               csv_demo_out)
})

test_that("read_demographics_csv() returns properly formatted data when date_only is set to FALSE", {
  expect_equal(read_demographics_csv(tempcsv_1, date_only = FALSE),
               csv_demo_out)
})

test_that("read_demographics_csv() can use a different version", {
  expect_equal(read_demographics_csv(tempcsv_2, date_only = FALSE),
               csv_demo_out)
})

test_that("read_demographics_csv() throws an error for an invalid version", {
  expect_error(read_demographics_csv(tempcsv_3, date_only = FALSE),
               "^invalid Envision csv version number: vERROR$")
})

file.remove(tempcsv_1)
file.remove(tempcsv_2)
file.remove(tempcsv_3)
