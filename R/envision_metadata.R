#' JAX Envision® Metadata
#'
#' This function allows you to create a time-saving metadata object that can be
#'  used as input in many of the \code{envisionR} functions. It checks the
#'  inputted metadata to ensure they make sense.
#'
#' @param study_name The name of the study (used for default figure legends)
#' @param tzone The time zone of the study
#' @param lights_on the 24-hour time for lights-on as a \code{character},
#'  \code{"HH:MM:SS"}
#' @param lights_off the 24-hour time for lights-off as a \code{character}
#' @param study_url the URL from the address bar of the study,
#'  a \code{character}. NOTE: this can be any URL taken from within a study.
#' @param override_tzassume override the default behavior of assuming a time
#'  zone? (default: \code{FALSE})
#' @param override_timecheck override the default behavior of assuming a time
#'  zone? (default: \code{FALSE})
#' @param force_lightcycle force a light/dark cycle that is less than 8 hours
#'  (defaults to \code{FALSE})
#' @param quietly suppress warning messages? (default: \code{FALSE})
#'
#' @note In the United States, the most common time zones are: `US/Eastern`,
#'  `US/Central`, `US/Mountain`, and `US/Pacific`. Some more uncommon US time
#'  zones are: `US/Alaska`, `US/Hawaii`, and `US/Arizona`.
#'
#' @note For Europe, the time zone `GB` will work for Great Britain, the
#'  time zone `WET` for Portugal, the time zone `CET` will work for much
#'  of central Europe, and `EET` will work for Finland, the Baltics, and many
#'  eastern European countries. For any other time zones, use the
#'  `list_tzones()`
#'
#' @note For any other time zones, the function `list_tzones()` will produce a
#'  data frame containing all valid time zone designations in the `tz_name`
#'  column.

envision_metadata <- function(study_name = "",
                              tzone = NULL,
                              lights_on = NULL,
                              lights_off = NULL,
                              study_url = NULL,
                              override_tzassume = FALSE,
                              override_timecheck = FALSE,
                              force_lightcycle = FALSE,
                              quietly = FALSE) {
  # checking for required namespace
  stopifnot(requireNamespace("hms"))
  stopifnot(requireNamespace("dplyr"))

  # defining global variables as NULL
  is_dst <- tz_name <- utc_offset_h <- assume <- NULL

  # checking time zone information
  if (!is.null(tzone)) {
    stopifnot(is.character(tzone))
    if (!override_tzassume) {
      tz_exists <- tzone %in% timezones_df$tz_name
      if (!tz_exists) {
        stop("time zone ", tzone, " is not a valid time zone name")
      }
    }
  } else {
    if (override_tzassume) {
      tzone <- as.character(NA)
      if (!quietly) {
        warning(
          "no time zone information, using NA"
        )
      }
    } else {
      sys_offset <- envisionR::get_utc_offset(Sys.time(), as_numeric = TRUE)
      sys_dst <- as.POSIXlt(Sys.time())$isdst == 1
      sys_assume <- timezones_df |>
        dplyr::filter(utc_offset_h == sys_offset &
          is_dst == sys_dst &
          assume == 1) |>
        dplyr::pull(tz_name)
      tzone <- sys_assume
      if (!quietly) {
        warning(
          "no time zone information, assuming the time zone: ",
          sys_assume, "\n"
        )
      }
    }
  }

  # checking lights-on and lights-off input
  if (!is.null(lights_on) & !is.null(lights_off)) {
    # check to see if it's a character vector
    stopifnot(is.character(lights_on))
    stopifnot(is.character(lights_off))

    # logic for finding whether or not the lights-on and lights-off make sense
    time_on <- try(hms::as_hms(lights_on), silent = TRUE)
    time_off <- try(hms::as_hms(lights_off), silent = TRUE)
    if (inherits(time_on, "try-error") | inherits(time_off, "try-error")) {
      stop(
        "reformat lights_on and/or lights_off as %HH:%MM:%SS (you entered ",
        lights_on, " for lights_on and ", lights_off, " for lights_off)\n"
      )
    }

    # find difference between light and dark
    lightdark_diff_s <- as.numeric(abs(time_off - time_on))

    # throw an error if lights-on and lights-off are separated by a very short time
    if (lightdark_diff_s %/% 3600 < 8) {
      if (!force_lightcycle) {
        stop("lights-on and lights-off separated by less than 8 hours, check input")
      } else if (force_lightcycle & !quietly) {
        warning("lights-on and lights-off separated by less than 8 hours, check input")
      }
    }
  } else if (xor(is.null(lights_on), is.null(lights_off))) {
    stop("only one value for lights_on and lights_off provided")
  } else {
    time_on <- hms::hms(NA)
    time_off <- hms::hms(NA)
    if (!quietly) {
      warning("no values given for lights_on or lights_off")
    }
  }

  # getting app url base
  if (!is.null(study_url)) {
    stopifnot(is.character(study_url))
    study_url <- gsub("^http[s]?://", "", study_url)
    study_url <- gsub("^(.*study/\\d+).*$", "\\1", study_url)
    if (grepl("^\\w+\\.\\w+\\.\\w{3}/org/\\d+/study/\\d+$", study_url)) {
      org <- as.numeric(gsub("^.*/org/(\\d+)/study/\\d+$", "\\1", study_url))
      study <- as.numeric(gsub("^.*/org/\\d+/study/(\\d+)$", "\\1", study_url))
    } else {
      stop(
        "study_url is not properly formed,",
        " ensure that contains has envision.jax.org/org/000/study/0000 ",
        "where 0s are replace by digits"
      )
    }
  } else {
    org <- as.numeric(NA)
    study <- as.numeric(NA)
  }

  # make the metadata object
  evmeta <- list(
    study_name = study_name,
    tzone = tzone,
    lights_on = time_on,
    lights_off = time_off,
    org = org,
    study = study
  )
  return(evmeta)
}
