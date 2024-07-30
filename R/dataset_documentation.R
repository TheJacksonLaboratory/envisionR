#' Dummy JAX Envision® annotation CSV lines formatted the same as exported annotation data.
#'
#' A dataset containing four lines of fabricated JAX Envision® annotation data.
#'  These lines follow the format of a JAX Envision® annotation CSV.
#'  When read properly, the lines for annotation data have column titles as follows:
#'
#'  \itemize{
#'    \item \code{id}. the ID of the annotation, usually a number.
#'    \item \code{created}. the date and time the annotation was created (coerced from UTC to local time).
#'    \item \code{created_date_local}. the date the annotation was created (in the time zone in which the data were collected).
#'    \item \code{created_time_local}. the time the annotation was created (in the time zone in which the data were collected).
#'    \item \code{pin_start_date_local}. the date of the start of the period to which the annotation refers (in the time zone in which the data were collected).
#'    \item \code{pin_start_time_local}. the time of the start of the period to which the annotation refers (in the time zone in which the data were collected).
#'    \item \code{pin_end_date_local}. the date of the end of the period to which the annotation refers (in the time zone in which the data were collected).
#'    \item \code{pin_end_time_local}. the time of the end of the period to which the annotation refers (in the time zone in which the data were collected).
#'    \item \code{study_code}. a unique code for each study.
#'    \item \code{group_name}. a user-defined group name, often used to label experimental groups of interest.
#'    \item \code{cage_name}. the name of the cage that the data represent.
#'    \item \code{creator}. the name of the user or bot who created the annotation.
#'    \item \code{contents}. the annotation.
#'    \item \code{reply_to}. if the annotation is a reply, the name of the user or bot to whom the reply is directed.
#'    \item \code{hashtags}. any hashtags in the annotation.
#'    \item \code{pin_start_time}. the date and time of the start of the period to which the annotation refers (in the time zone in which the data were collected).
#'    \item \code{pin_end_time}. athe date and time of the end of the period to which the annotation refers (in the time zone in which the data were collected).
#'    \item \code{tzone}. time zone of the dataset.
#'  }
#'
#' @docType data
#' @keywords datasets
#' @name annotation_csv_example
#' @usage data(annotation_csv_example)
#' @format a character vector with 5 lines, the first line is a header.
NULL

#' Dummy JAX Envision® cage-level activity CSV lines formatted the same as exported activity data.
#'
#' A dataset containing four lines of fabricated JAX Envision® activity data.
#'  These lines follow the format of a JAX Envision® cage-level activity CSV.
#'  When read properly, the lines for cage-level data have raw column titles as follows:
#'
#'  \itemize{
#'    \item \code{start}. the date and time at the start of the aggregation bin (coerced from UTC to local time).
#'    \item \code{start_date_local}. the start date (in the time zone in which the data were collected).
#'    \item \code{start_time_local}. the start time (in the time zone in which the data were collected).
#'    \item \code{study_code}. a unique code for each study.
#'    \item \code{aggregation_seconds}. the number of seconds aggregated to generate this dataset (3600 is 1 hour).
#'    \item \code{group_name}. a user-defined group name, often used to label experimental groups of interest.
#'    \item \code{cage_name}. the name of the cage that the data represent.
#'    \item \code{animals_cage_quantity}. the number of animals in the cage, sometimes called cage density or occupancy.
#'    \item \code{light_cycle}. whether the data were collected in the light or dark cycle.
#'    \item \code{movement_mean_per_cage_cm_s_hour}. cage-level movement in cm/s for a specified period of time (1 hour in this example)
#'    \item \code{wheel_occupancy_mean_per_cage_animals_hour}. amount of time spent on the wheel at the cage level.
#'    \item \code{food_occupancy_mean_per_cage_animals_hour}. amount of time spent in proximity to the food hopper at the cage level.
#'    \item \code{water_occupancy_mean_per_cage_animals_hour}. amount of time spent in proximity to the water bottles at the cage level.
#'    \item \code{tzone}. time zone of the dataset.
#'  }
#'
#' @docType data
#' @keywords datasets
#' @name activity_cage_csv_example
#' @usage data(activity_cage_csv_example)
#' @format a character vector with 5 lines, the first line is a header.
NULL

#' Dummy JAX Envision® animal-level activity CSV lines formatted the same as exported activity data.
#'
#' A dataset containing four lines of fabricated JAX Envision® activity data.
#'  These lines follow the format of a JAX Envision® animal-level activity CSV.
#'  When read properly, the lines for cage-level data have raw column titles as follows:
#'
#'  \itemize{
#'    \item \code{start}. the date and time at the start of the aggregation bin (coerced from UTC to local time).
#'    \item \code{start_date_local}. the start date (in the time zone in which the data were collected).
#'    \item \code{start_time_local}. the start time (in the time zone in which the data were collected).
#'    \item \code{study_code}. a unique code for each study.
#'    \item \code{aggregation_seconds}. the number of seconds aggregated to generate this dataset (3600 is 1 hour).
#'    \item \code{group_name}. a user-defined group name, often used to label experimental groups of interest.
#'    \item \code{cage_name}. the name of the cage that the data represent.
#'    \item \code{animals_cage_quantity}. the number of animals in the cage, sometimes called cage density or occupancy.
#'    \item \code{light_cycle}. whether the data were collected in the light or dark cycle.
#'    \item \code{animal.id}. the unique ID of the animal.
#'    \item \code{strain}. the strain of animal.
#'    \item \code{sex}. the sex of the animal.
#'    \item \code{genotype}. the genotype of the animal.
#'    \item \code{birth_date}. the date of birth for the animal.
#'    \item \code{movement_animal_cm_s_hour}. cage-level movement in cm/s for a specified period of time (1 hour in this example)
#'    \item \code{wheel_occupancy_animal_s.hour}. amount of time spent on the wheel at the cage level.
#'    \item \code{food_occupancy_animal_s_hour}. amount of time spent in proximity to the food hopper at the cage level.
#'    \item \code{water_occupancy_animal_s_hour}. amount of time spent in proximity to the water bottles at the cage level.
#'    \item \code{tzone}. time zone of the dataset.
#'  }
#'
#' @docType data
#' @keywords datasets
#' @name activity_animal_csv_example
#' @usage data(activity_animal_csv_example)
#' @format a character vector with 5 lines, the first line is a header.
NULL

#' Column definition
#'
#' Definitions for the columns of different Envision® csv types and versions.
#'
#' @docType data
#' @keywords datasets
#' @name csv_column_defs
#' @usage data(csv_column_defs)
#' @format a list with both cage-level and animal-level column specifications
NULL
