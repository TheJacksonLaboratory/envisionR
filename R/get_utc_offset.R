#' Get UTC Offset
#'
#' This function computes UTC offset for a given time point.
#' Getting a UTC offset can be useful when daylight time starts or stops.
#' @param ts time stamp in local time
#' @param ts_utc time stamp in UTC time (default: \code{NULL})
#' @param as_numeric whether UTC offset should be returned as a \code{numeric} (default: \code{FALSE}).
#' @returns A \code{character} or \code{numeric} UTC offset.
#' @keywords Envision
#' @export
#' @examples
#' # Use with no second time stamp and an explicit time zone
#' get_utc_offset(ts = lubridate::ymd_hms("2024-06-01 12:00:00", tz = "US/Pacific"))
#'
#' # Use with a second time stamp and no explicit time zone.
#' get_utc_offset(ts = lubridate::ymd_hms("2024-06-01 12:00:00"),
#'                ts_utc = lubridate::ymd_hms("2024-06-01 05:00:00"))

get_utc_offset = function(ts, ts_utc = NULL, as_numeric = FALSE) {

  # Throwing an error if the R installation does not include lubridate
  stopifnot(requireNamespace("lubridate", quietly = TRUE))

  # Throwing an error if timestamp is not a timepoint
  stopifnot(lubridate::is.timepoint(ts))

  if (!is.null(ts_utc)) {
    stopifnot(lubridate::is.timepoint(ts_utc))
  } else {
    ts_utc <- lubridate::with_tz(ts, tzone = "UTC")
  }

  # Finding UTC time and using it to produce offset
  offset_hr <- lubridate::as.period(lubridate::force_tz(ts, tzone = "UTC") -
                                      ts_utc) %/% lubridate::hours(1)
  offset_mn <- lubridate::as.period(lubridate::force_tz(ts, tzone = "UTC") -
                                      ts_utc) %% lubridate::hours(1) %/%
    lubridate::minutes(1)

  # Getting character or numeric output
  if (as_numeric) {
    offset = offset_hr + offset_mn / 60
  } else {
    offset = paste0(formatC(offset_hr, width = 3, flag = "0+"), ":",
                    formatC(abs(offset_mn), width = 2, flag = "0"))
  }
  # Returning a properly formatted UTC offset as a character vector
  return(offset)
}
