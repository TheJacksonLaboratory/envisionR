reaggregate = function(activity_1min,
                       measures,
                       timespan_sec,
                       offset_sec) {
  measures_timespan = gsub("_min$","_timespan",measures)
  measures_timespan_cage = gsub("_animal_","_cage_",measures_timespan)

  reaggregation = activity_1min |>
    dplyr::mutate(aggregation_seconds = timespan_sec,
                  start_time_local_timespan = as.numeric(start_time_local) - (as.numeric(start_time_local) + offset_sec) %% (timespan_sec),
                  start_date_local = as.Date(ifelse(start_time_local_timespan < 0, start_date_local - 1, start_date_local)),
                  start_time_local_timespan = hms::as_hms(ifelse(start_time_local_timespan < 0, 86400 + start_time_local_timespan, start_time_local_timespan)),
                  start_timespan = lubridate::ymd_hms(paste0(as.character(start_date_local), " ", as.character(start_time_local_timespan)),
                                                      tz = metaactivity_1min$tzone)) |>
    dplyr::group_by(sex, strain, animal_id, cage_name,
                    genotype, group_name, study_code, aggregation_seconds,
                    start_time_local_timespan, start_timespan) |>
    dplyr::arrange(start) |>
    dplyr::summarize(across(all_of(measures),
                            ~ mean(.x, na.rm = TRUE),
                            .names = "{sub('_min$','_timespan',.col)}"))
  return(reaggregation)
}
