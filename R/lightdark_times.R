#' Get light/dark times
#'
#' This function returns a \code{tibble} of lights-off and lights-on times.
#' @param lights_on_time Time of lights-on, a string formatted as \code{"HH:MM"}
#' @param lights_off_time Time of lights-off, a string formatted as \code{"HH:MM"}
#' @param start_date First dates in the series of dates, a string formatted as \code{"YYYY-MM-DD"}
#' @param end_date Last date in the series of dates, a string formatted as \code{"YYYY-MM-DD"}
#' @param tzone The time zone, which should match a time zone given in the \code{list_tzones()} function.
#' @returns A \code{tibble} with lights-off and lights-on times for all dates, useful for constructing figures.
#' @keywords Envision
#' @export
#' @examples
#' lightdark_times(
#'   lights_on_time = "06:00",
#'   lights_off_time = "18:00",
#'   start_date = "2024-06-02",
#'   end_date = "2024-06-03",
#'   tzone = "US/Pacific"
#' )
lightdark_times <- function(lights_on_time, lights_off_time,
                            start_date, end_date,
                            tzone = "UTC") {
  # Requiring dplyr for the namespace
  stopifnot(requireNamespace("tibble", quietly = TRUE))

  # Getting grep results for the lights-on and lights-off times
  lights_onoff_regex <- "^[012][0123456789]:[012345][0123456789]$"
  lights_on_grep <- grep(lights_onoff_regex, lights_on_time)
  lights_off_grep <- grep(lights_onoff_regex, lights_off_time)

  # Checking times for correct formatting
  if (length(lights_on_grep) != 1 | length(lights_off_grep) != 1) {
    stop("lights-on and lights-off times should be formatted as HH:MM and be in 24 hour time.")
  }

  # Getting a grep for the dates
  dates_regex <- "^\\d{4}-\\d{2}-\\d{2}$"
  start_date_grep <- grep(dates_regex, start_date)
  end_date_grep <- grep(dates_regex, end_date)

  # Checking dates for correct formatting
  if (length(start_date_grep) != 1 | length(end_date_grep) != 1) {
    stop("start date and end date should be formatted as YYYY-MM-DD.")
  }

  # Turning times into decimals and running checks
  lights_on_hr_decimal <- as.numeric(gsub(
    "^(\\d{1,2}):(\\d{1,2})$",
    "\\1", lights_on_time
  ))
  lights_on_mn_decimal <- as.numeric(gsub(
    "^(\\d{1,2}):(\\d{1,2})$",
    "\\2", lights_on_time
  ))
  lights_off_hr_decimal <- as.numeric(gsub(
    "^(\\d{1,2}):(\\d{1,2})$",
    "\\1", lights_off_time
  ))
  lights_off_mn_decimal <- as.numeric(gsub(
    "^(\\d{1,2}):(\\d{1,2})$",
    "\\2", lights_off_time
  ))
  stopifnot(lights_on_hr_decimal < 23 | lights_on_hr_decimal > 0)
  stopifnot(lights_off_hr_decimal < 23 | lights_off_hr_decimal > 0)
  stopifnot(lights_on_mn_decimal < 59 | lights_on_mn_decimal > 0)
  stopifnot(lights_off_mn_decimal < 59 | lights_off_mn_decimal > 0)
  lights_on_decimal <- lights_on_hr_decimal + lights_on_mn_decimal / 60
  lights_off_decimal <- lights_off_hr_decimal + lights_off_mn_decimal / 60

  # Running some checks on

  # Running a few checks.
  stopifnot(!is.na(as.Date(start_date)))
  stopifnot(!is.na(as.Date(end_date)))
  stopifnot(
    "time zone must be in time zone database" =
      tzone %in% timezones_df$tz_name
  )
  stopifnot(as.POSIXct(paste(start_date, lights_on_time)) < as.POSIXct(paste(end_date, lights_on_time)))

  # Making sequence of light-dark times
  if (lights_on_decimal < lights_off_decimal) {
    lights_on_dates <- seq(
      from = as.Date(start_date),
      to = as.Date(end_date) + 1,
      by = 1
    )
    lights_off_dates <- lights_on_dates - 1
  } else if (lights_off_decimal < lights_on_decimal) {
    lights_on_dates <- seq(
      from = as.Date(start_date),
      to = as.Date(end_date),
      by = 1
    )
    lights_off_dates <- lights_on_dates
  } else {
    stop("lights-on and lights-off must be different times.")
  }

  # Making character vectors
  lights_on_char <- paste(
    as.character(lights_on_dates),
    as.character(lights_on_time)
  )
  lights_off_char <- paste(
    as.character(lights_off_dates),
    as.character(lights_off_time)
  )

  # Making date-time vectors
  lights_on_times <- as.POSIXct(lights_on_char, tz = tzone)
  lights_off_times <- as.POSIXct(lights_off_char, tz = tzone)

  # Making a tibble with these data
  lightdark_df <- data.frame(
    lights_off = lights_off_times,
    lights_on = lights_on_times
  ) |>
    tibble::as_tibble()

  # Returning the data frame
  return(lightdark_df)
}
