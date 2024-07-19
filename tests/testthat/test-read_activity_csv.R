# Unit tests for the read_activity_csv() function.

# Making dummy CSV file
csv_lines_1 = "start,start.date.local,start.time.local,study.code,aggregation.seconds,group.name,cage.name,animals.cage.quantity,light.cycle,movement.mean.per_cage.cm_s.hour,wheel_occupancy.mean.per_cage.s.hour,food_occupancy.mean.per_cage.s.hour,water_occupancy.mean.per_cage.s.hour
2023-11-01 23:00:00+00:00,2023-11-01,16:00:00,test1,3600,G1,A1,3,Light,2,0,0.003,0
2023-11-02 00:00:00+00:00,2023-11-01,17:00:00,test1,3600,G1,A1,3,Light,2.8,0.025,0.004,0.0002
2023-11-02 01:00:00+00:00,2023-11-01,18:00:00,test1,3600,G1,A1,3,Dark,6,0.1,0.07,0.012
2023-11-02 02:00:00+00:00,2023-11-01,19:00:00,test1,3600,G1,A1,3,Dark,5.5,0.08,0.1,0.008"

# Writing out the dummy CSV file
csv_lines_1 = unlist(strsplit(csv_lines_1, "\n"))
tempcsv_1 = tempfile("testactivity", fileext = ".csv")
writeLines(csv_lines_1, tempcsv_1)

# Writing out the CSV file that should throw an unambiguous time code error
csv_lines_error = gsub("19:00:00","22:00:00",csv_lines_1)
tempcsv_2 = tempfile("testactivity_error", fileext = ".csv")
writeLines(csv_lines_error, tempcsv_2)

# Making the data frame with variable types that should match the wrangled CSV file
csv_out = read.csv(text = csv_lines_1) |>
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
