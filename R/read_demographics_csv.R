#' A Function to Read JAX Envision® animal and cage demographics CSVs
#'
#' This function allows you to read animal and cage demographics CSVs generated
#'  by the JAX Envision® software.
#' @name read_demographics_csv
#' @param csv File path for the Envision® CSV.
#' @param date_only Whether to return date or date-time for \code{birth_date}
#'  and \code{death_date} fields. Default: \code{TRUE}, which converts these
#'  fields to a date.
#' @returns A \code{tibble} with experimental data optimally formatted for
#'  downstream analysis.
#' @keywords Envision
#' @export
#' @examples
#' # Writing test CSV file
#' demographics_csv <- tempfile("testdemographics", fileext = ".csv")
#' readr::write_lines(demographics_csv_example, file = demographics_csv)
#'
#' # Reading in test CSV file
#' demographics <- read_demographics_csv(csv = demographics_csv)
#'
#' # Glimpsing demographics CSV
#' dplyr::glimpse(demographics)
#'
#' # Reading in test CSV file using the wrapper function read_animalscages_csv()
#' animalscages <- read_animalscages_csv(csv = demographics_csv)
#'
#' # Glimpsing demographics CSV
#' dplyr::glimpse(animalscages)
#'
#' # Reading in test CSV file using the wrapper function read_animalsandcages_csv()
#' animalsandcages <- read_animalsandcages_csv(csv = demographics_csv)
#'
#' # Glimpsing demographics CSV
#' dplyr::glimpse(animalsandcages)
#'
#' # Removing samples from workspace
#' rm(list = c("demographics",
#'             "animalscages",
#'             "animalsandcages"))
#'
#' # Removing the demographics temp file
#' file.remove(demographics_csv)

# NOTE: should make a wrapper function called read_animalsandcages_csv()
#' @rdname read_demographics_csv
#' @order 1
#' @export
read_demographics_csv <- function(csv, date_only = TRUE) {

  # Ensuring required packages are loaded
  stopifnot(requireNamespace("readr", quietly = TRUE))
  stopifnot(requireNamespace("janitor", quietly = TRUE))
  stopifnot(requireNamespace("dplyr", quietly = TRUE))
  stopifnot(requireNamespace("tibble", quietly = TRUE))

  # Doing variable bindings
  birth_date <- death_date <- NULL

  # Looking for a version of the activity CSV
  first10_csv <- base::readLines(csv, n = 10)

  # Looking at starting comment characters within the first 10 lines of the CSV
  # NOTE: this is to ensure that the comment character can be read in the body
  # of the sheet.
  comment_char_csv <- grep("^#", first10_csv, value = TRUE)
  demographics_csv_version <- grep("[Vv]ersion",
                               comment_char_csv,
                               value = TRUE)
  header_col_csv <- grep("^start,", first10_csv, value = TRUE)

  # Getting version number
  if (length(demographics_csv_version) == 0) {
    demographics_csv_version <- "v0.0.0.9000"
  } else {
    demographics_csv_version <- paste0("v",
                                   gsub("^.*[Vv]ersion[:]?\\s?(.*)\\s?.?$",
                                        "\\1", demographics_csv_version))
    if (!demographics_csv_version %in% envisionR::csv_column_defs$version_numbers) {
      stop("invalid Envision csv version number: ", demographics_csv_version)
    }
  }

  demo_cols <- envisionR::csv_column_defs[["demographics"]][[demographics_csv_version]]

  # Reading in raw data
  demo_data <- readr::read_csv(csv, skip = length(comment_char_csv),
                               col_types = demo_cols,
                               show_col_types = FALSE) |>
    janitor::clean_names() |>
    tibble::as_tibble()

  if (date_only) {
      demo_data <- demo_data |>
        dplyr::mutate(birth_date = as.Date(birth_date),
                      death_date = as.Date(death_date))
  }

  return(demo_data)
}

# Wrapper function for read_demographics_csv()
#' @rdname read_demographics_csv
#' @order 2
#' @export
read_animalscages_csv <- function(csv, date_only = TRUE) {
  return(read_demographics_csv(csv, date_only))
}

# Wrapper function for read_demographics_csv()
#' @rdname read_demographics_csv
#' @order 3
#' @export
read_animalsandcages_csv <- function(csv, date_only = TRUE) {
  return(read_demographics_csv(csv, date_only))
}
