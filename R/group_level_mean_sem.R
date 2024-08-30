#' A Function to Compute Group-Level Summary Statistics on JAX Envision® activity CSVs
#'
#' This function computes mean, standard error, and error bars for activity data.
#' @param activity_data a cage-level activity dataset.
#' @param var the activity metric to summarize.
#'  (defaults to \code{"movement_mean_per_cage_cm_s_hour"})
#' @param by time varible to summarize by (defaults to \code{"start"})
#' @param occupancy_normalize should the function normalize by cage occupancy?
#'  (defaults to \code{TRUE})
#' @param occupancy_weighted should the function weight by cage occupancy?
#'  (defaults to \code{TRUE})
#' @param na.rm should the function remove \code{NA} values?
#'  (default: \code{TRUE})
#' @param quietly should the function suppress warnings?
#'  (default: \code{FALSE})
#' @returns summary statistics for the dataset
#' @keywords Envision
#' @export
#' @examples
#' # Getting dummy dataset
#' data("activity_cage_data_example")
#'
#' # Computing summary statistics
#' x <- group_level_mean_sem(activity_cage_data_example)
#'
#' # Looking at the summary statistics
#' head(x)
group_level_mean_sem <- function(activity_data,
                                 var = "movement_mean_per_cage_cm_s_hour",
                                 by = "start",
                                 occupancy_normalize = TRUE,
                                 occupancy_weighted = TRUE,
                                 na.rm = TRUE,
                                 quietly = FALSE) {
  group_name <- mean_activity <- sem_activity <- animals_cage_quantity <- NULL
  activity_data <- as.data.frame(activity_data)
  dataset_occunorm_col <- grep("^occupancy_norm",
    colnames(activity_data),
    value = TRUE
  )

  if (length(dataset_occunorm_col) != 0) {
    is_normalized <- sum(activity_data[, dataset_occunorm_col]) == nrow(activity_data)
  } else {
    is_normalized <- FALSE
  }

  if (occupancy_normalize) {
    if (is_normalized) {
      activity_data[, "var"] <- activity_data[, var]
      if (!quietly) {
        warning("dataset already occupancy normalized")
      }
    } else {
      activity_data[, "var"] <- ifelse(activity_data[, "animals_cage_quantity"] != 0,
        activity_data[, var] / activity_data[, "animals_cage_quantity"],
        NA
      )
    }
  } else {
    activity_data[, "var"] <- activity_data[, var]
  }

  if (!occupancy_weighted) {
    activity_summary <- activity_data |>
      dplyr::group_by(group_name, start) |>
      dplyr::summarize(
        mean_activity = mean(var, na.rm = na.rm),
        sem_activity = sd(var, na.rm = na.rm) / sqrt(length(which(!is.na(var)))),
        ll_activity = mean_activity - sem_activity,
        ul_activity = mean_activity + sem_activity
      ) |>
      tibble::as_tibble()
  } else {
    activity_summary <- activity_data |>
      dplyr::group_by(group_name, start) |>
      dplyr::summarize(
        mean_activity = weighted.mean(
          x = var,
          w = animals_cage_quantity,
          na.rm = na.rm
        ),
        sem_activity = weighted_sem(
          x = var,
          w = animals_cage_quantity,
          na.rm = na.rm
        ),
        ll_activity = mean_activity - sem_activity,
        ul_activity = mean_activity + sem_activity
      ) |>
      tibble::as_tibble()
  }
  return(activity_summary)
}
