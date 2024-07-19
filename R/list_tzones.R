#' A Function to list time zones compatible with the JAX Envision® software
#'
#' This function allows you list possible time zones for the JAX Envision® software.
#' @returns A \code{data.frame} with compatible time zones in the `tz_name` column.
#' @keywords Envision
#' @export
#' @examples
#' list_tzones()

list_tzones = function() {

  # Getting right libraries
  stopifnot(require(dplyr))

  # Grabbing the data frame
  tzones = envisionR:::timezones |>
    dplyr::select(-assume, -override, -zone)

  return(tzones)
}
