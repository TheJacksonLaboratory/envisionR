# Unit tests for the read_activity_csv() function.

# Writing out a dummy CSV file
tempcsv_1 = tempfile("testactivity", fileext = ".csv")
writeLines(csv_lines, tempcsv_1)

# Writing out the CSV file that should throw an unambiguous time code error
csv_lines_error = gsub("19:00:00","22:00:00",csv_lines)
tempcsv_2 = tempfile("testactivity_error", fileext = ".csv")
writeLines(csv_lines_error, tempcsv_2)

# Making the data frame with variable types that should match the wrangled CSV file
csv_out = read.csv(text = csv_lines) |>
  tibble::as_tibble() |>
  janitor::clean_names() |>
  dplyr::mutate(start = lubridate::ymd_hms(start, tz = "US/Pacific", quiet = TRUE),
                start_date_local = lubridate::ymd(start_date_local),
                start_time_local = hms::as_hms(start_time_local),
                study_code = as.character(study_code),
                aggregation_seconds = as.numeric(paste0(aggregation_seconds, ".0")),
                group_name = as.character(group_name),
                cage_name = as.character(cage_name),
                animals_cage_quantity = as.numeric(paste0(animals_cage_quantity, ".0")),
                light_cycle = as.character(light_cycle),
                movement_mean_per_cage_cm_s_hour = as.numeric(movement_mean_per_cage_cm_s_hour),
                wheel_occupancy_mean_per_cage_s_hour = as.numeric(wheel_occupancy_mean_per_cage_s_hour),
                food_occupancy_mean_per_cage_s_hour = as.numeric(food_occupancy_mean_per_cage_s_hour),
                water_occupancy_mean_per_cage_s_hour = as.numeric(water_occupancy_mean_per_cage_s_hour),
                tzone = "US/Pacific")

test_that("read_activity_csv() returns expected tibble with default parameters", {
  expect_equal(read_activity_csv(tempcsv_1, tz = "US/Pacific"),
               csv_out)
})

test_that("read_activity_csv() returns no warnings with default parameters and matching time zones", {
          expect_no_warning(read_activity_csv(tempcsv_1, tz = "US/Pacific"))
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
