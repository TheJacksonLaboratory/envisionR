#' Experiment time encoding
#'
#' This function uses the date-time information in an annotation or an activity data set to compute experimental time.
#' Experimental time is useful in aligning disparate studies and for re-encoding by something like date of birth.
#'
#' @param rawtimes raw times to be changed to experiment times.
#' @param reftimes reference time or times for an experiment time encoding. Either length 1 or the same length as \code{rawtimes}.
#' @param units units for the experiment time encoding (hours, days, weeks, months, or years).
#' @param offset value given for the offset from 0 in the experiment in the same units as \code{units} (default: \code{0}).
#' @returns a \code{vector} of experiment time in the given units.
#' @keywords Envision

experiment_time_encoding = function(rawtimes,
                                    reftimes,
                                    units = c("seconds","minutes","hours",
                                              "days","weeks","months","years"),
                                    offset = 0) {

  # Ensuring essential packages are in the namespace
  stopifnot(requireNamespace("lubridate", quietly = TRUE))

  # Ensuring that times are all instants
  stopifnot(lubridate::is.instant(rawtimes))
  stopifnot(lubridate::is.instant(reftimes))

  # Ensuring that the reftime value is numeric
  stopifnot(is.numeric(offset))

  # Ensuring that reftimes matches rawtimes
  if (length(reftimes) == 1 & length(rawtimes) != 1) {
    reftimes = rep(reftimes, times = length(rawtimes))
  } else if (length(reftimes) == length(rawtimes) & length(reftimes) != 1) {
    reftimes = reftimes
  } else {
    stop("rawtimes and reftimes are different lengths")
  }

  # Getting different numbers depending upon scale
  if (units == "seconds") {
    # 1 second is the interval unit encoding
    newtimes_raw <- as.double(lubridate::interval(reftimes, rawtimes))
  } else if (units == "minutes") {
    # 1 minute = 60 seconds
    newtimes_raw <- as.double(lubridate::interval(reftimes, rawtimes) /
                                60)
  } else if (units == "hours") {
    # 1 hour = 60 * 60 = 3600 seconds
    newtimes_raw <- as.double(lubridate::interval(reftimes, rawtimes) /
                                3600)
  } else if (units == "days") {
    # 1 day = 24 * 60 * 60 = 86400 seconds
    newtimes_raw <- as.double(lubridate::interval(reftimes, rawtimes) /
                                86400)
  } else if (units == "weeks") {
    # 1 week = 7 * 24 * 60 * 60 = 604800 seconds
    newtimes_raw <- as.double(lubridate::interval(reftimes, rawtimes) /
                                604800)
  } else if (units == "months") {
    # 1 month = (365.25 * 24 * 60 * 60) / 12 = 2629800 seconds
    newtimes_raw <- as.double(lubridate::interval(reftimes, rawtimes) /
                                2629800)
  } else if (units == "years") {
    # 1 year = 365.25 * 24 * 60 * 60 = 31557600 seconds
    newtimes_raw <- as.double(lubridate::interval(reftimes, rawtimes) /
                                31557600)
  }

  # Adding the reference time value
  newtimes = newtimes_raw + offset

  # Returning the result
  return(newtimes)
}
