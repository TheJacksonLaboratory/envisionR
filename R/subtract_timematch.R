#' @export
subtract_timematch <- function(activity_data,
                               var = "movement_mean_per_cage_cm_s_hour",
                               occupancy_normalize = TRUE,
                               quietly = FALSE) {
  df_i <- unique_cages <- cage_name <- aggregation_seconds <- group_name <-
    subtract_var <- animals_cage_quantity <- occupancy_norm <- hour <-
    internal_na <- impute <- NULL
  stopifnot(requireNamespace("dplyr"))
  stopifnot(requireNamespace("tibble"))
  activity_data <- as.data.frame(activity_data)
  if (occupancy_normalize) {
    activity_data[, "raw"] <- ifelse(activity_data[, "animals_cage_quantity"] != 0,
      activity_data[, var] / activity_data[, "animals_cage_quantity"],
      NA
    )
  } else {
    activity_data[, "raw"] <- activity_data[, var]
  }

  unique_cages <- activity_data |>
    dplyr::pull(cage_name) |>
    unique()
  warn_internal_na <- FALSE

  for (i in unique_cages) {
    # making a sequence of times at the finest resolution in the dataset
    min_start_cage_i <- activity_data |>
      dplyr::filter(cage_name == i) |>
      dplyr::pull(start) |>
      min(na.rm = TRUE)
    max_start_cage_i <- activity_data |>
      dplyr::filter(cage_name == i) |>
      dplyr::pull(start) |>
      max(na.rm = TRUE)
    time_i <- activity_data |>
      dplyr::filter(cage_name == i) |>
      dplyr::pull(aggregation_seconds) |>
      min(na.rm = TRUE)
    seq_cage_i <- seq(
      from = min_start_cage_i,
      to = max_start_cage_i,
      by = time_i
    )
    df_i <- data.frame(
      start = seq_cage_i,
      cage_name = i,
      subtract_var = var,
      occupancy_norm = occupancy_normalize
    ) |>
      dplyr::left_join(activity_data, by = c("cage_name", "start")) |>
      dplyr::select(
        start, cage_name, group_name, subtract_var,
        animals_cage_quantity, occupancy_norm, raw
      ) |>
      dplyr::mutate(
        hour = format(start, "%H:%M:%S"),
        leading_trailing_na = FALSE
      )
    leading_nas <- head(which(!is.na(df_i[, "raw"])), 1)
    trailing_nas <- tail(which(!is.na(df_i[, "raw"])), 1)
    rows_i <- seq_len(nrow(df_i))
    leading_trailing_nas_rows <- rows_i[which(rows_i < leading_nas | rows_i > trailing_nas)]
    df_i[leading_trailing_nas_rows, "leading_trailing_na"] <- TRUE
    df_i[, "internal_na"] <- is.na(df_i[, "raw"]) & !(df_i[, "leading_trailing_na"])
    impute_i <- which(df_i[, "internal_na"])
    warn_internal_na <- ifelse(length(impute_i > 0), TRUE, warn_internal_na)
    # imputing internal NAs
    df_i <- df_i |>
      dplyr::group_by(hour) |>
      dplyr::mutate(impute = ifelse(internal_na,
        base::mean(raw, na.rm = TRUE),
        raw
      )) |>
      dplyr::ungroup()

    if (i == unique_cages[1]) {
      cage_sub_df <- df_i
    } else {
      cage_sub_df <- rbind(
        cage_sub_df,
        df_i
      )
    }
  }
  cage_sub_df <- cage_sub_df |>
    dplyr::ungroup() |>
    dplyr::arrange(cage_name, hour, start) |>
    dplyr::group_by(cage_name, hour) |>
    dplyr::mutate(time_matched_subtracted = impute - dplyr::lag(impute, 1)) |>
    dplyr::ungroup() |>
    dplyr::arrange(group_name, cage_name, start) |>
    dplyr::as_tibble()

  return(cage_sub_df)
}
