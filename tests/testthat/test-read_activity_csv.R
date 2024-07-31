# Unit tests for the read_activity_csv() function.

# Getting required libraries
require("tibble")
require("janitor")
require("dplyr")
require("hms")

# Writing out a dummy CSV file (cage-level)
tempcsv_1 = tempfile("testactivity", fileext = ".csv")
writeLines(activity_cage_csv_example, tempcsv_1)

# Writing out the CSV file that should throw an unambiguous time code error
csv_lines_error = gsub("19:00:00","22:00:00",activity_cage_csv_example)
tempcsv_2 = tempfile("testactivity_error", fileext = ".csv")
writeLines(csv_lines_error, tempcsv_2)

# Writing out the CSV file that should work if vTEST is included
tempcsv_3 = tempfile("testactivity_version", fileext = ".csv")
writeLines(c("# Version: TEST",activity_cage_csv_example), tempcsv_3)

# Writing out the CSV file that should work if vTEST is included
tempcsv_4 = tempfile("testactivity_version_error", fileext = ".csv")
writeLines(c("# Version: ERROR",activity_cage_csv_example), tempcsv_4)

# Writing out the CSV file that should throw an unambiguous time code error
csv_header_error = gsub("cage", "notright", activity_cage_csv_example)
tempcsv_5 = tempfile("testactivity_header_error", fileext = ".csv")
writeLines(csv_header_error, tempcsv_5)

# Writing out a dummy CSV file (animal-level)
tempcsv_6 = tempfile("testactivity", fileext = ".csv")
writeLines(activity_animal_csv_example, tempcsv_6)

# Making the data frame with variable types that should match the wrangled CSV file
csv_cage_out = read.csv(text = activity_cage_csv_example) |>
  tibble::as_tibble() |>
  janitor::clean_names() |>
  dplyr::mutate(start = as.POSIXct(as.POSIXct(start, tz = "UTC"), tz = "US/Pacific"),
                start_date_local = as.Date(start_date_local),
                start_time_local = hms::as_hms(start_time_local),
                study_code = as.character(study_code),
                aggregation_seconds = as.numeric(paste0(aggregation_seconds, ".0")),
                group_name = as.character(group_name),
                cage_name = as.character(cage_name),
                animals_cage_quantity = as.integer(animals_cage_quantity),
                light_cycle = as.character(light_cycle),
                movement_mean_per_cage_cm_s_hour = as.numeric(movement_mean_per_cage_cm_s_hour),
                wheel_occupancy_mean_per_cage_s_hour = as.numeric(wheel_occupancy_mean_per_cage_s_hour),
                food_occupancy_mean_per_cage_s_hour = as.numeric(food_occupancy_mean_per_cage_s_hour),
                water_occupancy_mean_per_cage_s_hour = as.numeric(water_occupancy_mean_per_cage_s_hour),
                tzone = "US/Pacific")

csv_animal_out = read.csv(text = activity_animal_csv_example) |>
  tibble::as_tibble() |>
  janitor::clean_names() |>
  dplyr::mutate(start = as.POSIXct(as.POSIXct(start, tz = "UTC"), tz = "US/Pacific"),
                start_date_local = as.Date(start_date_local),
                start_time_local = hms::as_hms(start_time_local),
                study_code = as.character(study_code),
                aggregation_seconds = as.numeric(paste0(aggregation_seconds, ".0")),
                group_name = as.character(group_name),
                cage_name = as.character(cage_name),
                animals_cage_quantity = as.integer(animals_cage_quantity),
                light_cycle = as.character(light_cycle),
                animal_id = as.character(animal_id),
                strain = as.character(strain),
                sex = as.character(sex),
                genotype = as.character(genotype),
                birth_date = as.Date(birth_date),
                movement_animal_cm_s_hour = as.numeric(movement_animal_cm_s_hour),
                wheel_occupancy_animal_s_hour = as.numeric(wheel_occupancy_animal_s_hour),
                food_occupancy_animal_s_hour = as.numeric(food_occupancy_animal_s_hour),
                water_occupancy_animal_s_hour = as.numeric(water_occupancy_animal_s_hour),
                tzone = "US/Pacific")

# Doing the tests
test_that("read_activity_csv() returns expected tibble with default parameters", {
  expect_equal(read_activity_csv(tempcsv_1, tz = "US/Pacific"),
               csv_cage_out)
})

test_that("read_activity_csv() returns no warnings with default parameters and matching time zones", {
          expect_no_warning(read_activity_csv(tempcsv_1, tz = "US/Pacific"))
})

test_that("read_activity_csv() outputs start in the correct time zone for cage-level metrics", {
  expect_equal(read_activity_csv(tempcsv_1, tz = "US/Pacific") |>
                 mutate(start = as.character(start)),
               csv_cage_out |>
                 mutate(start = as.character(start)))
})

test_that("read_activity_csv() throws a warning when time zone is omitted", {
  expect_warning(read_activity_csv(tempcsv_1),
                 regexp = "^Assuming time zone: \\w+/\\w+\\. Set time zone explicitly if different.$")
})

test_that("read_activity_csv() throws a warning when started time zone mismatches UTC offset", {
  expect_warning(read_activity_csv(tempcsv_1, tz = "Canada/Newfoundland"),
                 regexp = "^UTC offset for the \\w+/\\w+ time zone mismatches suggested time zones.$")
})

test_that("read_activity_csv() throws an error when started time zone does not exist", {
  expect_error(read_activity_csv(tempcsv_1, tz = "Canada/New_Foundland"),
               regexp = "^time zone \\w+/\\w+ is not a system time zone.$")
})

test_that("read_activity_csv() throws an error when multiple irreconcilable UTC offsets are present", {
  expect_error(read_activity_csv(tempcsv_2),
               regexp = "^could not assume a time zone unambiguously.$")
})

test_that("read_activity_csv() works with vTEST", {
  expect_equal(read_activity_csv(tempcsv_3, "US/Pacific"),
               csv_cage_out)
})

test_that("read_activity_csv() throws an error when version is not in the column definitions file", {
  expect_error(read_activity_csv(tempcsv_4),
               regexp = "^invalid Envision csv version number: vERROR$")
})

test_that("read_activity_csv() throws an error when neither animal nor cage is in the column header", {
  expect_error(read_activity_csv(tempcsv_4),
               regexp = "^invalid Envision csv version number: vERROR$")
})

test_that("read_activity_csv() throws an error when neither animal nor cage is in the column header", {
  expect_error(read_activity_csv(tempcsv_5),
               regexp = "^metrics should be defined as either cage or animal level$")
})

# Doing the tests
test_that("read_activity_csv() returns expected tibble with animal-level metrics", {
  expect_equal(read_activity_csv(tempcsv_6, tz = "US/Pacific"),
               csv_animal_out)
})

test_that("read_activity_csv() outputs start in the correct time zone for animal-level metrics", {
  expect_equal(read_activity_csv(tempcsv_6, tz = "US/Pacific") |>
                 mutate(start = as.character(start)),
               csv_animal_out |>
                 mutate(start = as.character(start)))
})


# Removing temp files
file.remove(tempcsv_1)
file.remove(tempcsv_2)
file.remove(tempcsv_3)
file.remove(tempcsv_4)
file.remove(tempcsv_5)
file.remove(tempcsv_6)

