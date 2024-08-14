#' @export
group_level_mean_sem <- function(activity_data,
                                 var = "movement_mean_per_cage_cm_s_hour",
                                 by = "start",
                                 occupancy_normalize = TRUE,
                                 occupancy_weighted = TRUE,
                                 na.rm = TRUE,
                                 quietly = FALSE) {
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
        sem_activity = sd(var, na.rm = na.rm) / length(which(!is.na(var))),
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
