#' Get UTC Offset
#'
#' This function computes UTC offset for a given time point.
#' Getting a UTC offset can be useful when daylight time starts or stops.
#' @param ts Time stamp in local time
#' @param as_numeric Whether UTC offset should be returned as a \code{numeric} (default: \code{FALSE}).
#' @returns A \code{character} or \code{numeric} UTC offset.
#' @keywords Envision
#' @export
#' @examples
#' get_utc_offset(ts = lubridate::ymd_hms("2024-06-01 12:00:00", tz = "US/Pacific"))

get_utc_offset = function(ts, as_numeric = FALSE) {

  # Throwing an error if the R installation does not include lubridate
  stopifnot(require("lubridate"))

  # Throwing an error if timestamp is not a timepoint
  stopifnot(lubridate::is.timepoint(ts))

  # Finding UTC time and using it to produce offset
  utc_ts    <- lubridate::with_tz(ts, tzone = "UTC")
  offset_hr <- lubridate::hour(ts) - lubridate::hour(utc_ts)
  offset_mn <- lubridate::minute(ts) - lubridate::minute(utc_ts)

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
