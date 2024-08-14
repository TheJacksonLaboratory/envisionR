#' @export
tsd_cagemetrics <- function(activity_data,
                            var = "movement_mean_per_cage_cm_s_hour",
                            occupancy_normalize = TRUE,
                            quietly = FALSE) {
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
    pull(cage_name) |>
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
    cage_df_i <- data.frame(
      start = seq_cage_i,
      cage_name = i,
      tsd_var = var,
      occupancy_norm = occupancy_normalize
    ) |>
      dplyr::left_join(activity_data, by = c("cage_name", "start")) |>
      dplyr::select(
        start, cage_name, group_name, tsd_var,
        animals_cage_quantity, occupancy_norm, raw
      ) |>
      dplyr::mutate(
        hour = format(start, "%H:%M:%S"),
        leading_trailing_na = FALSE
      )
    leading_nas <- head(which(!is.na(cage_df_i[, "raw"])), 1)
    trailing_nas <- tail(which(!is.na(cage_df_i[, "raw"])), 1)
    rows_i <- seq_len(nrow(cage_df_i))
    leading_trailing_nas_rows <- rows_i[which(rows_i < leading_nas | rows_i > trailing_nas)]
    cage_df_i[leading_trailing_nas_rows, "leading_trailing_na"] <- TRUE
    cage_df_i[, "internal_na"] <- is.na(cage_df_i[, "raw"]) & !(cage_df_i[, "leading_trailing_na"])
    impute_i <- which(cage_df_i[, "internal_na"])
    warn_internal_na <- ifelse(length(impute_i > 0), TRUE, warn_internal_na)
    cage_df_i <- cage_df_i |>
      dplyr::group_by(hour) |>
      dplyr::mutate(impute = ifelse(internal_na, mean(raw, na.rm = TRUE), raw)) |>
      dplyr::ungroup() |>
      dplyr::select(-hour, -leading_trailing_na, -internal_na)
    ts_i <- ts(pull(cage_df_i, impute),
      frequency = 86400 / time_i
    )
    decomp_i <- decompose(ts_i, type = "additive")
    cage_df_i[, "circadian"] <- as.numeric(decomp_i[["seasonal"]])
    cage_df_i[, "trend"] <- as.numeric(decomp_i[["trend"]])
    cage_df_i[, "residual"] <- as.numeric(decomp_i[["random"]])
    cage_df_i[, "detrended"] <- cage_df_i[, "raw"] - as.numeric(decomp_i[["seasonal"]])
    cage_df_i <- tibble::as_tibble(cage_df_i)
    if (i == unique_cages[1]) {
      cage_tsd_df <- cage_df_i
    } else {
      cage_tsd_df <- rbind(
        cage_tsd_df,
        cage_df_i
      )
    }
  }
  if (warn_internal_na & !quietly) {
    warning("internal NA values imputed in this dataset")
  }
  return(tibble::as_tibble(cage_tsd_df))
}
