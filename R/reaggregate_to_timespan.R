reaggregate_to_timespan = function(activity,
                                   measures) {
  measures_timespan = gsub("_min$","_timespan",measures)
  measures_timespan_cage = gsub("_animal_","_cage_",measures_timespan)

  summary = activity |>
    dplyr::group_by(sex, strain, cage_name,
                    genotype, group_name, study_code, aggregation_seconds,
                    start_time_local_timespan, start_timespan) |>
    dplyr::summarize(across(all_of(measures_timespan),
                            ~ mean(.x, na.rm = TRUE),
                            .names = "{sub('_animal_','_cage_',.col)}")) |>
    dplyr::ungroup() |>
    dplyr::group_by(sex, strain,
                    genotype, group_name, study_code, aggregation_seconds,
                    start_time_local_timespan, start_timespan) |>
    dplyr::summarize(across(all_of(measures_timespan_cage),
                            ~ mean(.x, na.rm =TRUE),
                            .names = "{.col}_mean"),
                     across(all_of(measures_timespan_cage),
                            ~ sd(.x, na.rm =TRUE),
                            .names = "{.col}_sd"),
                     across(all_of(measures_timespan_cage),
                            ~ length(which(!is.na(.x))),
                            .names = "{.col}_n")) |>
    dplyr::mutate(across(ends_with("_sd"),
                         ~ .x / sqrt(get(sub("_sd$","_n",cur_column()))),
                         .names = "{sub('_sd$', '_sem', .col)}"),
                  across(ends_with("_sem"),
                         ~  get(sub("_sem$", "_mean", cur_column())) - .x,
                         .names = "{sub('_sem$', '_ll', .col)}"),
                  across(ends_with("_sem"),
                         ~  get(sub("_sem$", "_mean", cur_column())) + .x,
                         .names = "{sub('_sem$', '_ul', .col)}")) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(
      cols = -c(sex, strain, genotype, group_name, study_code, aggregation_seconds,
                start_time_local_timespan, start_timespan),
      names_to = c("measure", "statistic"),
      names_pattern = "(.*)_(mean|sd|n|sem|ll|ul)$",
      values_to = "value"
    ) |>
    dplyr::mutate(start_date_local_timespan = date(start_timespan),
                  timespan_hrs = aggregation_seconds / 3600,
                  tzone = metadata$tzone,
                  group_stat = paste0(group_name, "_", statistic),
                  measure = gsub("_timespan$","",measure)) |>
    dplyr::select(-strain, -genotype, -group_name, -statistic) |>
    tidyr::pivot_wider(
      id_cols = c(start_timespan, start_date_local_timespan,
                  start_time_local_timespan, timespan_hrs, tzone,
                  study_code, sex, measure),
      names_from = group_stat,
      values_from = value
    )
  colnames(summary) = gsub("_timespan$","",colnames(summary))
  return(summary)
}
