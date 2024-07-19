#' Make a URL for the JAX Envision® platform
#'
#' This function creates a URL for the JAX Envision® platform.
#' URLs are useful for highlighting specific moments of video identified by an analysis.
#' @param org Organization number of the video.
#' @param study Study number of the video.
#' @param cage Cage number for the video.
#' @param vidstart Start time of the video (usually a \code{timepoint} or \code{instant} created with the \code{lubridate} package).
#' @param widnowstart_h Hours before the start of the video for the window.
#' @param widnowstart_h Hours after the start of the video for the window.
#' @param metricstab Which metrics tab to return (either \code{"cage"}, \code{"animal"}, or \code{"alpha"}).
#' @param url_base What URL base is used for the Envision® URL: NOTE: the default of \code{"https://app.murine.net/org/"} is from the original version of the app.
#' @param videostream Which video stream to return (note: for now, \code{"overlay"} is the default).
#' @returns A \code{character} vector containing the JAX Envision® URL.
#' @keywords Envision
#' @export
#' @examples
#' make_envision_url()

make_envision_url = function(org, study, cage, vidstart,
                             windowstart_h = 12, windowend_h = 12,
                             metricstab = "cage", videostream = "overlay",
                             url_base = "https://app.murine.net/org/") {

  # Checking each argument
  stopifnot(metricstab %in% c("cage","animal","alpha"))
  stopifnot(videostream %in% c("overlay"))
  stopifnot(is.instant(vidstart))
  stopifnot(is.numeric(org))
  stopifnot(is.numeric(study))
  stopifnot(is.numeric(cage))
  stopifnot(is.numeric(windowstart_h) & sign(windowstart_h) == 1)
  stopifnot(is.numeric(windowend_h) & sign(windowend_h) == 1)

  unix_time = as.numeric(vidstart)
  timestamp = format(1000 * unix_time, scientific = FALSE)
  ws = format(unix_time * 1000 - windowstart_h * 3.6e6, scientific = FALSE)
  we = format(unix_time * 1000 + windowend_h * 3.6e6, scientific = FALSE)
  url_end = paste0(cage, "?metricsTab=", metricstab, "&rangeEnd=",
                   we, "&rangeStart=", ws, "&videoStart=", timestamp,
                   "&videoStream=", videostream)

  return(paste(url_base, org, "study", study, "cage", url_end, sep = "/"))
}
