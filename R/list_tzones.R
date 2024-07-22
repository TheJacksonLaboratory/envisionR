#' A Function to list time zones compatible with the JAX Envision® software
#'
#' This function allows you list possible time zones for the JAX Envision® software.
#' @returns A \code{data.frame} with compatible time zones in the `tz_name` column.
#' @keywords Envision
#' @export
#' @examples
#' list_tzones()

list_tzones = function() {

  # Getting dplyr library
  stopifnot(requireNamespace("dplyr", quietly = TRUE))

  # Doing variable bindings
  assume <- override <- zone <- NULL

  # Grabbing the data frame
  tzones = timezones_df |>
    dplyr::select(-assume, -override, -zone)

  return(tzones)
}
