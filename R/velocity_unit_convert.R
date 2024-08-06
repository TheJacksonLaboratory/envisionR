#' Convenience function for simple velocity unit conversion
#'
#' This function allows you to convert between velocity units.
#'
#' @param x The velocity as a \code{numeric} vector
#' @param units_in The velocity units in
#' @param units_out The velocity units out
#'
#' The units should be in \code{distance/time} format.
#'  Valid values for distance are:
#'  \itemize{
#'    \item \code{mm}: millimeters
#'    \item \code{cm}: centimeters
#'    \item \code{m}: meters
#'    \item \code{km}: kilometers
#'    \item \code{in}: inches
#'    \item \code{ft}: feet
#'    \item \code{yd}: yards
#'  }
#'  Valid values for time are:
#'  \itemize{
#'    \item \code{s}: second
#'    \item \code{m}: minute
#'    \item \code{h}: hour
#'  }
#'
#' @export
velocity_unit_convert <- function(x, units_in = "cm/s", units_out = "cm/s") {
  # Doing a check on input
  stopifnot("x must be numeric" = is.numeric(x))
  stopifnot("units_in must be a character" = is.character(units_in))
  stopifnot("units_in must be length 1" = length(units_in) == 1)
  stopifnot(
    "units_in must have format dd/tt where dd is distance and tt is time" =
      grepl("^\\w{1,2}/\\w{1,2}$", units_in)
  )
  stopifnot("units_out must be a character" = is.character(units_out))
  stopifnot("units_out must be length 1" = length(units_out) == 1)
  stopifnot(
    "units_out must have format dd/tt where dd is distance and tt is time" =
      grepl("^\\w{1,2}/\\w{1,2}$", units_out)
  )

  # Getting distance and time values
  dist_in <- gsub("^(\\w{1,2})/(\\w{1,2})$", "\\1", units_in)
  time_in <- gsub("^(\\w{1,2})/(\\w{1,2})$", "\\2", units_in)
  dist_out <- gsub("^(\\w{1,2})/(\\w{1,2})$", "\\1", units_out)
  time_out <- gsub("^(\\w{1,2})/(\\w{1,2})$", "\\2", units_out)

  # Doing some checks
  stopifnot(
    "numerator of units_in must be either mm, cm, m, km, in, ft, or yd" =
      dist_in %in% c("mm", "cm", "m", "km", "in", "ft", "yd")
  )
  stopifnot(
    "numerator of units_out must be either mm, cm, m, km, in, ft, or yd" =
      dist_out %in% c("mm", "cm", "m", "km", "in", "ft", "yd")
  )
  stopifnot(
    "denominator of units_in must be either s, m, or h" =
      time_in %in% c("s", "m", "h")
  )
  stopifnot(
    "denominator of units_out must be either s, m, or h" =
      time_out %in% c("s", "m", "h")
  )

  # changing dist_in and dist_out to inches if it's in (which is a reserved
  # variable name in R)
  dist_in <- gsub("^in$", "inches", dist_in)
  dist_out <- gsub("^in$", "inches", dist_out)

  # getting distance multiplier
  dist_num <- switch(dist_in,
    km = {
      1e6
    },
    m = {
      1000
    },
    cm = {
      10
    },
    mm = {
      1
    },
    inches = {
      25.4
    },
    ft = {
      304.8
    },
    yd = {
      914.4
    }
  )
  dist_den <- switch(dist_out,
    km = {
      1e6
    },
    m = {
      1000
    },
    cm = {
      10
    },
    mm = {
      1
    },
    inches = {
      25.4
    },
    ft = {
      304.8
    },
    yd = {
      914.4
    }
  )
  dist_mult <- dist_num / dist_den

  # getting time multiplier
  time_num <- switch(time_out,
    s = {
      1
    },
    m = {
      60
    },
    h = {
      3600
    }
  )
  time_den <- switch(time_in,
    s = {
      1
    },
    m = {
      60
    },
    h = {
      3600
    }
  )
  time_mult <- time_num / time_den

  return(x * time_mult * dist_mult)
}
