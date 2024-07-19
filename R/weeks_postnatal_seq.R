#' Make a sequence of weeks postnatal
#'
#' This function computes a vector of dates that start each postnatal weeks after a birth date.
#' This is often useful for axis legends.
#' @param dob Date of birth of the animal in a \code{ymd} format.
#' @param n_weeks Number of weeks as a \code{numeric}.
#' @returns A named vector of dates where the names are the postnatal weeks.
#' @keywords Envision
#' @export
#' @examples
#' weeks_postnatal_seq(dob = lubridate::ymd("2024-01-20"), n_weeks = 12)

weeks_postnatal_seq <- function(dob, n_weeks) {

  # Throwing an error if lubridate is not available
  stopifnot(requireNamespace("lubridate", quietly = TRUE))

  # Throwing an error if DOB is not a Date
  stopifnot(lubridate::is.Date(dob))

  # Throwing an error if number of n_weeks is not numeric
  stopifnot(is.numeric(n_weeks))

  # Getting a sequence of postnatal days at which an animal is a certain age
  pnds <- seq(from = dob, to = dob + 7 * floor(n_weeks), by = 7)
  names(pnds) <- paste0("Postnatal Week ", seq_len(length(pnds)) - 1)

  # Returning the vector
  return(pnds)
}
