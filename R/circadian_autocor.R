fullday_autocor = function(activity_subsetted,
                           focal_hour,
                           cage_name,
                           animal_id = NA,
                           daylag = 1,
                           tzone = "US/Eastern",
                           metrics_col = "activity_per_cage_cm_s_hour") {

  # Stop execution if focal_hour is not POSIXct
  stopifnot(is.POSIXct(focal_hour))

  # Stop execution if MASS isn't present
  stopifnot(require("MASS", quietly = TRUE))

  local_focal = hms::as_hms(lubridate::with_tz(focal_hour, tzone = tzone))

  # Ensure activity_subsetted is a data frame and has the required columns
  activity_subsetted = as.data.frame(activity_subsetted)
  stopifnot(metrics_col %in% colnames(activity_subsetted))

  # Make new column with name metrics_col
  activity_subsetted$metrics_col = activity_subsetted[,metrics_col]

  # Get two data frames with 24 hours of data
  hours_dm0 = activity_subsetted |>
    dplyr::filter(start <= focal_hour & start > focal_hour - lubridate::hours(24)) |>
    dplyr::select(start_time_local, metrics_col)
  hours_dm1 = activity_subsetted |>
    dplyr::filter(start <= focal_hour - daylag * lubridate::hours(24) & start > focal_hour - (daylag + 1) * lubridate::hours(24)) |>
    dplyr::select(start_time_local, metrics_col) |>
    dplyr::rename(dm1 = metrics_col)

  # Join the data frames
  hours_dates = hours_dm0 |>
    dplyr::left_join(hours_dm1, by = "start_time_local") |>
    as.data.frame()

  # Create data frame to return
  cor_df = data.frame(cage_name = cage_name,
                      animal_id = animal_id,
                      start = as.character(focal_hour),
                      daylag = daylag,
                      n = length(which(rowSums(is.na(hours_dates[,c("metrics_col","dm1")])) == 0)),
                      cor = cor(hours_dates$metrics_col,
                                hours_dates$dm1,
                                use = "pairwise.complete.obs"))
  return(cor_df)
}

# Function to calculate rolling autocorrelation of activity data on a full cage-level dataset
rolling_cage_autocor = function(activity,
                                metadata,
                                metrics_col = "activity_per_cage_cm_s_hour",
                                exclude_cage_change = FALSE,
                                cage_change_dates,
                                minhours = 6) {

  # Initialize empty data frame to store results
  cages_df = data.frame(cage_name = character(),
                        start = character(),
                        daylag = numeric(),
                        n = numeric(),
                        cor = numeric())

  # Loop through all cages in the activity data
  for (cage_i in unique(activity$cage_name)) {

    activity_i = activity |>
      dplyr::filter(cage_name == cage_i) |>
      as.data.frame()

    # Exclude cage change periods if specified
    if (exclude_cage_change) {
      cage_change_starts <- as.POSIXct(paste0(as.character(cage_change_dates), " ",
                                              as.character(metadata$lights_on)),
                                       tz = metadata$tzone)
      cage_change_stops <- cage_change_starts + 86400

      for (i in seq_len(length(cage_change_dates))) {
        start_i <- cage_change_starts[i]
        stop_i <- cage_change_stops[i]
        startstop_i <- which(activity_i$start >= start_i & activity_i$start < stop_i)
        activity_i[startstop_i, metrics_col] <- NA
      }
    }

    # Get hours in dataset
    hours = seq(from = min(activity_i$start),
                to = max(activity_i$start),
                by = "hour")

    # Run rolling 24-hour autocorrelation for each hour in the cage data
    for (hour_i in hours) {
      hour_i2 = lubridate::as_datetime(hour_i, tz = "UTC")

      # Calculate the autocorrelations for 1, 2, and 3 days lagged
      df_i = rbind(fullday_autocor(activity_i,
                                   cage_name = cage_i,
                                   focal_hour = hour_i2,
                                   daylag = 1,
                                   tzone = metadata$tzone,
                                   metrics_col = metrics_col),
                   fullday_autocor(activity_i,
                                   cage_name = cage_i,
                                   focal_hour = hour_i2,
                                   daylag = 2,
                                   tzone = metadata$tzone,
                                   metrics_col = metrics_col),
                   fullday_autocor(activity_i,
                                   cage_name = cage_i,
                                   focal_hour = hour_i2,
                                   daylag = 3,
                                   tzone = metadata$tzone,
                                   metrics_col = metrics_col))

      # Add the results to the cages_df
      cages_df = rbind(cages_df,
                       df_i)

    }
  }

  # Convert start to POSIXct and filter cages with sufficient data
  cages_df = cages_df |>
    dplyr::mutate(cor = ifelse(n < minhours, NA, cor),
                  start = lubridate::as_datetime(start),
                  start = lubridate::with_tz(start, tzone = metadata$tzone),
                  daylag_factor = factor(daylag,
                                         levels = c(1, 2, 3),
                                         labels = c("1 day", "2 days", "3 days"),
                                         ordered = TRUE)) |>
    dplyr::group_by(cage_name, start)

  # Return the cages_df
  return(cages_df)
}

# Function to calculate rolling autocorrelation of activity data on a full cage-level dataset
rolling_animal_autocor = function(activity,
                                  focalhour,
                                  metadata,
                                  metrics_col = "activity_per_animal_cm_s_hour",
                                  exclude_cage_change = FALSE,
                                  cage_change_dates,
                                  minhours = 6) {

  # Initialize empty data frame to store results
  animals_df = data.frame(animal_id = character(),
                          start = character(),
                          daylag = numeric(),
                          n = numeric(),
                          cor = numeric())

  # Loop through all cages in the activity data
  for (animal_i in unique(activity$animal_id)) {

    activity_i = activity |>
      dplyr::filter(animal_id == animal_i) |>
      as.data.frame()

    cage_i = activity_i |>
      dplyr::pull(cage_name) |>
      unique()

    # Exclude cage change periods if specified
    if (exclude_cage_change) {
      cage_change_starts <- as.POSIXct(paste0(as.character(cage_change_dates), " ",
                                              as.character(metadata$lights_on)),
                                       tz = metadata$tzone)
      cage_change_stops <- cage_change_starts + 86400

      for (i in seq_len(length(cage_change_dates))) {
        start_i <- cage_change_starts[i]
        stop_i <- cage_change_stops[i]
        startstop_i <- which(activity_i$start >= start_i & activity_i$start < stop_i)
        activity_i[startstop_i, metrics_col] <- NA
      }
    }

    # Get hours in dataset
    hours = seq(from = min(activity_i$start),
                to = max(activity_i$start),
                by = "hour")

    # Run rolling 24-hour autocorrelation for each hour in the cage data
    for (hour_i in hours) {
      hour_i2 = lubridate::as_datetime(hour_i, tz = "UTC")

      # Calculate the autocorrelations for 1, 2, and 3 days lagged
      df_i = rbind(fullday_autocor(activity_i,
                                   cage_name = cage_i,
                                   animal_id = animal_i,
                                   focal_hour = hour_i2,
                                   daylag = 1,
                                   tzone = metadata$tzone,
                                   metrics_col = metrics_col),
                   fullday_autocor(activity_i,
                                   animal_id = animal_i,
                                   cage_name = cage_i,
                                   focal_hour = hour_i2,
                                   daylag = 2,
                                   tzone = metadata$tzone,
                                   metrics_col = metrics_col),
                   fullday_autocor(activity_i,
                                   animal_id = animal_i,
                                   cage_name = cage_i,
                                   focal_hour = hour_i2,
                                   daylag = 3,
                                   tzone = metadata$tzone,
                                   metrics_col = metrics_col))

      # Add the results to the cages_df
      animals_df = rbind(animals_df,
                         df_i)

    }
  }

  # Convert start to POSIXct and filter cages with sufficient data
  animals_df = animals_df |>
    dplyr::mutate(cor = ifelse(n < minhours, NA, cor),
                  start = lubridate::as_datetime(start),
                  start = lubridate::with_tz(start, tzone = metadata$tzone),
                  daylag_factor = factor(daylag,
                                         levels = c(1, 2, 3),
                                         labels = c("1 day", "2 days", "3 days"),
                                         ordered = TRUE)) |>
    dplyr::group_by(cage_name, start)

  # Return the animals_df
  return(animals_df)
}

