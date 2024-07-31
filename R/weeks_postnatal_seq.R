#' Make a sequence of weeks postnatal
#'
#' This function computes a vector of dates that start each postnatal weeks after a birth date.
#' This is often useful for axis legends.
#' @param dob Date of birth of the animal in a \code{Date} format.
#' @param n_weeks Number of weeks as a \code{numeric}.
#' @param quiet \code{logical} indicating whether or not to suppress errors (default: \code{FALSE})
#' @returns A named vector of dates where the names are the postnatal weeks.
#' @keywords Envision
#' @export
#' @examples
#' weeks_postnatal_seq(dob = as.Date("2024-01-20"), n_weeks = 12)
weeks_postnatal_seq <- function(dob, n_weeks, quiet = FALSE) {
  # Throwing an error if DOB is not a POSIXct or cannot be coerced to one
  if (!inherits(dob, "Date") & !is.character(dob)) {
    stop("dob is neither a Date nor a character vector")
  } else if (is.character(dob)) {
    date_dob <- try(as.Date(dob), silent = TRUE)
    if (inherits(date_dob, "try-error")) {
      stop("dob cannot be coerced to a Date")
    } else {
      if (!quiet) {
        warning("dob coerced from character to Date type")
      }
      dob <- date_dob
    }
  }

  # Throwing an error if number of n_weeks is not numeric
  stopifnot(is.numeric(n_weeks))

  # Getting a sequence of postnatal days at which an animal is a certain age
  pnds <- seq(
    from = dob,
    to = dob + 7 * floor(n_weeks),
    by = 7
  )
  names(pnds) <- paste0("Postnatal Week ", seq_len(length(pnds)) - 1)

  # Returning the vector
  return(pnds)
}
