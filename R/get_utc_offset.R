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
#' get_utc_offset(ts = as.POSIXct("2024-06-01 12:00:00", tzone = "US/Pacific"))
#'
#' # Use with a second time stamp and no explicit time zone.
#' get_utc_offset(ts = as.POSIXct("2024-06-01 12:00:00"),
#'                ts_utc = as.POSIXct("2024-06-01 05:00:00"))

get_utc_offset = function(ts, ts_utc = NULL, as_numeric = FALSE) {

  # Throwing an error if timestamp is not a POSIXct
  stopifnot(inherits(ts, "POSIXct"))

  if (!is.null(ts_utc)) {
    stopifnot(inherits(ts_utc, "POSIXct"))
  } else {
    ts_utc <- ts
    attributes(ts_utc)$tzone <- "UTC"
  }

  # Forcing the initial time zone to a new time zone
  char_ts <- format(ts, "%Y-%m-%d %H:%M:%S")
  force_utc_ts <- as.POSIXct(char_ts, tz = "UTC")
  diff_secs <- as.double(force_utc_ts) - as.double(ts_utc)

  # Finding UTC time and using it to produce offset
  offset_hr <- abs(diff_secs) %/% 3600 * sign(diff_secs)
  offset_mn <- abs(diff_secs) %% 3600 %/% 60 * sign(diff_secs)

  # Getting character or numeric output
  if (as_numeric) {
    offset <- offset_hr + offset_mn / 60
  } else {
    offset <- paste0(formatC(offset_hr, width = 3, flag = "0+"), ":",
                     formatC(abs(offset_mn), width = 2, flag = "0"))
  }
  # Returning a properly formatted UTC offset as a character vector
  return(offset)
}
