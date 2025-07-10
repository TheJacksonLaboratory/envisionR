## ----import_libraries---------------------------------------------------------
#| warning: false
# R code

# Importing libraries
library(tidyverse)
library(janitor)
library(ggplot2)
library(here)
library(envisionR)


## ----make_url-----------------------------------------------------------------
# R code

# Getting a time as a POSIXct object
# NOTE: time zone must be correct to refer to the right moment
example_vidstart <- as.POSIXct("2024-06-17 09:00:00",
                               tz = "US/Central")

# Using a function to generate an Envision URL
make_envision_url(org = 9,
                  study = 237,
                  cage = 1823,
                  vidstart = example_vidstart)


## ----metadata-----------------------------------------------------------------
# R code

# Getting study metadata
metadata <- envisionR::envision_metadata(study_name = "Caffeine vs. Vehicle Study",
                                         tzone = "US/Central", # The time zone of the study
                                         lights_on = "06:00:00", # When lights-on happens in the study
                                         lights_off = "18:00:00", # When lights-off happens in the study
                study_url = "https://envision.jax.org/org/9/study/237/")


## ----set_random_seed----------------------------------------------------------
# R code

# Setting random seed
set.seed(20250117)


## ----import_annotation--------------------------------------------------------
# R code

# Getting the annotation path
annot_path <- system.file("extdata", "annotation.csv", package = "envisionR")

# Opening annotation file
annotation <- envisionR::read_annotation_csv(annot_path,
                                             metadata = metadata)


## ----glimpse_annotation-------------------------------------------------------
# R code

# Glimpsing annotation dataset
dplyr::glimpse(annotation)


## ----extract_grouping_variables_1---------------------------------------------
# R code

# Extracting dose data
annotation_group <- annotation |>
  dplyr::filter(grepl("[Dd]os[ei]", contents) & 
                !grepl("^@", contents) &
                grepl("insert", contents)) |>
  dplyr::mutate(drug = ifelse(grepl("[Vv]ehicle", contents), "Vehicle", ""),
                drug = ifelse(grepl("[Cc]affeine", contents), "Caffeine", drug),
                dose = ifelse(drug == "Vehicle", 0, -100),
                dose = ifelse(grepl("16", contents), 16, dose)) 


## ----make_drug_table_1--------------------------------------------------------
# R code

# Making a table with the drug and dose information
annotation_group |>
  dplyr::group_by(drug, dose) |>
  dplyr::summarize(n = length(contents))


## ----make_drug_table_3--------------------------------------------------------
# R code

annotation_group <- annotation_group |>
  group_by(cage_name) |>
  summarize(drug = unique(drug),
            dose = unique(dose),
            drug_dose_mgperkg = paste(str_to_title(drug), " (", dose, 
                                      " mg/kg)", sep = ""),
            dose1_time = pin_start_time[which(pin_start_time < ymd_hms("2024-06-18 18:00:00"))],
            dose2_time = pin_start_time[which(pin_start_time > ymd_hms("2024-06-18 18:00:00"))])


## ----annotation_grouping_check------------------------------------------------
# R code

annotation_group


## ----make_serialize-----------------------------------------------------------
# R code

# Making folder to hold serialized R objects
# NOTE: this function throws a warning if the directory already exists
outdir = tempdir()

# Serializing annotation
saveRDS(annotation, paste0(outdir, "/annotation.RDS"))
saveRDS(annotation_group, paste0(outdir, "/annotation_group.RDS"))


## ----import_cage_data---------------------------------------------------------
# R code

# Getting the demographic data path
demo_path <- system.file("extdata", "cagedata.csv", package = "envisionR")

# Importing cage data
demodata <- envisionR::read_demographics_csv(demo_path)


## ----glimpse_cage_data--------------------------------------------------------
# R code

# Glimpsing cage data
dplyr::glimpse(demodata)


## ----serialize_cagedata-------------------------------------------------------
# R code

# Serializing the cagedata data frame
saveRDS(demodata, paste0(outdir, "/demodata.RDS"))


## ----import_activity----------------------------------------------------------
# R code

# Getting the activity data path
act_path <- system.file("extdata", "activity.csv", package = "envisionR")

# Importing the activity data and cleaning column names.
activity <- envisionR::read_activity_csv(act_path,
                                         metadata = metadata)


## ----activity_change_tz-------------------------------------------------------
# R code

# Glimpsing the dataset again
dplyr::glimpse(activity)


## ----summarize_activity-------------------------------------------------------
# R code

# Summarizing activity
summary(activity)


## ----join_activity_grouping---------------------------------------------------
# R code

# Doing a left join of activity and annotation_group
activity <- activity |>
  dplyr::left_join(annotation_group, by = "cage_name")


## ----convert_to_m_h-----------------------------------------------------------
# R code

activity_mperh <- activity |>
  dplyr::mutate(movement_mean_per_cage_m_h_hour = velocity_unit_convert(movement_mean_per_cage_cm_s_hour,
                                                                        units_in = "cm/s",
                                                                        units_out = "m/h"))

dplyr::glimpse(activity_mperh)


## ----eval_activity_occupancy--------------------------------------------------
# R code

# Making a table of cage occupancy
table(activity$animals_cage_quantity, activity$cage_name)


## ----visualize_cages_occupancy------------------------------------------------
# R code

# Visualizing whether there are occupancy issues.
lightdark_violin_plot(activity_data = activity,
                      metadata = metadata,
                      visualize_on = "minoccupancy",
                      yvar = "movement_mean_per_cage_cm_s_hour")


## ----correcting_occupancy-----------------------------------------------------
# R code

# Computing a per animal normalized metric
activity_cagenorm <- activity|>
  dplyr::mutate(movement_mean_per_animal_cm_s_hour = movement_mean_per_cage_cm_s_hour / animals_cage_quantity)


## ----visualize_occupancy_changes----------------------------------------------
# R code

# Visualizing change in occupancy.
lightdark_violin_plot(activity_data = activity_cagenorm,
                      metadata = metadata,
                      visualize_on = "minoccupancy",
                      yvar = "movement_mean_per_animal_cm_s_hour")


## ----serialize_activity_data--------------------------------------------------
# R code

# Serializing the cleaned activity data.
saveRDS(activity, paste0(outdir, "/activity.RDS"))


## ----view_okabe_ito, echo=FALSE, warning=FALSE--------------------------------
# R code

# Showing the Okabe-Ito color palette
scales::show_col(ggokabeito::palette_okabe_ito())


## ----ggokabeito---------------------------------------------------------------
# R code

# Getting R libraries
library(ggokabeito)
okabe_order = c(5,8,6,1,3,4,2,7,9)


## ----visualize_grouping_boxwhiskers-------------------------------------------
# R code

lightdark_violin_plot(activity_data = activity_cagenorm,
                      metadata = metadata,
                      visualize_on = "group_name",
                      yvar = "movement_mean_per_animal_cm_s_hour") +
  scale_color_okabe_ito(order = okabe_order)


## ----spaghetti_plot-----------------------------------------------------------
# R code

spaghetti_plot(activity_data = activity, 
               metadata = metadata,
               yvar = "movement_mean_per_cage_cm_s_hour",
               occupancy_norm = TRUE) +
  ggokabeito::scale_color_okabe_ito(order = okabe_order)


## ----ribbon_plot_unweighted---------------------------------------------------
# R code

group_mean_sem_ribbon_plot(activity_data = activity,
                           metadata = metadata,
                           yvar = "movement_mean_per_cage_cm_s_hour",
                           occupancy_norm = TRUE,
                           occupancy_weight = FALSE) + 
  scale_color_okabe_ito(order = okabe_order) + 
  scale_fill_okabe_ito(order = okabe_order)


## ----ribbon_plot_weighted-----------------------------------------------------
# R code

group_mean_sem_ribbon_plot(activity_data = activity,
                           metadata = metadata,
                           yvar = "movement_mean_per_cage_cm_s_hour",
                           occupancy_norm = TRUE,
                           occupancy_weight = TRUE) + 
  scale_color_okabe_ito(order = okabe_order) + 
  scale_fill_okabe_ito(order = okabe_order)


## ----spaghetti_30hr-----------------------------------------------------------
# R code

# Getting x limits for first dose
xlimits_firstdose <- as.POSIXct(c("2024-06-17 00:00:00", "2024-06-18 06:00:00"),
                                tz = metadata[["tzone"]])

spaghetti_plot(activity_data = activity, 
               metadata = metadata,
               yvar = "movement_mean_per_cage_cm_s_hour",
               occupancy_norm = TRUE,
               xlim = xlimits_firstdose) +
  ggokabeito::scale_color_okabe_ito(order = okabe_order)


## ----ribbon_30hr--------------------------------------------------------------
# R code

group_mean_sem_ribbon_plot(activity_data = activity,
                           metadata = metadata,
                           yvar = "movement_mean_per_cage_cm_s_hour",
                           occupancy_norm = TRUE,
                           occupancy_weight = TRUE,
                           xlim = xlimits_firstdose) + 
  scale_color_okabe_ito(order = okabe_order) + 
  scale_fill_okabe_ito(order = okabe_order)


## ----subtract_timematch-------------------------------------------------------
# R code

# Subtracting time matched data from the day before study days and visualizing the results
activity_timematchsubtract <- subtract_timematch(activity_data = activity,
                                                 var = "movement_mean_per_cage_cm_s_hour",
                                                 occupancy_normalize = TRUE)

dplyr::glimpse(activity_timematchsubtract)


## ----subtract_spaghetti-------------------------------------------------------
# R code

spaghetti_plot(activity_data = activity_timematchsubtract,
               metadata = metadata,
               yvar = "time_matched_subtracted",
               occupancy_norm = FALSE) +
    scale_color_okabe_ito(order = okabe_order) 


## ----subtract_ribbon----------------------------------------------------------
# R code

group_mean_sem_ribbon_plot(activity_data = activity_timematchsubtract,
                           metadata = metadata,
                           yvar = "time_matched_subtracted",
                           occupancy_norm = FALSE) +
  scale_color_okabe_ito(order = okabe_order) +
  scale_fill_okabe_ito(order = okabe_order)


## ----subtract_ribbon_xlim-----------------------------------------------------
# R code

group_mean_sem_ribbon_plot(activity_data = activity_timematchsubtract,
                           metadata = metadata,
                           yvar = "time_matched_subtracted",
                           occupancy_norm = FALSE,
                           xlim = xlimits_firstdose) +
  scale_color_okabe_ito(order = okabe_order) +
  scale_fill_okabe_ito(order = okabe_order)


## ----activity_tsd_cagemetrics-------------------------------------------------
# R code

activity_tsd <- tsd_cagemetrics(activity_data = activity,
                                var = "movement_mean_per_cage_cm_s_hour",
                                occupancy_normalize = TRUE)
glimpse(activity_tsd)


## ----spaghetti_plot_circadian-------------------------------------------------
# R code

spaghetti_plot(activity_data = activity_tsd,
               metadata = metadata,
               yvar = "circadian",
               occupancy_norm = FALSE) +
  ggokabeito::scale_color_okabe_ito(order = okabe_order)


## ----detrended_ribbon_plot----------------------------------------------------
# R code

group_mean_sem_ribbon_plot(activity_data = activity_tsd,
                           metadata = metadata,
                           yvar = "detrended",
                           occupancy_norm = FALSE) +
  ggokabeito::scale_color_okabe_ito(order = okabe_order) +
  ggokabeito::scale_fill_okabe_ito(order = okabe_order)


## ----detrended_ribbon_plot_zoomed---------------------------------------------
# R code

group_mean_sem_ribbon_plot(activity_data = activity_tsd,
                           metadata = metadata,
                           yvar = "detrended",
                           occupancy_norm = FALSE,
                           xlim = xlimits_firstdose) +
  ggokabeito::scale_color_okabe_ito(order = okabe_order) +
  ggokabeito::scale_fill_okabe_ito(order = okabe_order)


## ----import_1min_data---------------------------------------------------------
# R code

# Getting the activity 1 minute data path
act1m_path <- system.file("extdata", "activity_1min.csv", package = "envisionR")

activity_1min <- read_activity_csv(act1m_path,
                                   metadata = metadata)

dplyr::glimpse(activity_1min)


## ----plot_spaghetti_1min------------------------------------------------------
# R code

spaghetti_plot(activity_data = activity_1min,
               metadata = metadata,
               yvar = "movement_mean_per_cage_cm_s_min",
               occupancy_norm = TRUE) +
  ggokabeito::scale_color_okabe_ito(order = okabe_order)


## ----ribbon_plot_1min_raw-----------------------------------------------------
# R code

group_mean_sem_ribbon_plot(activity_data = activity_1min,
                           metadata = metadata,
                           yvar = "movement_mean_per_cage_cm_s_min",
                           occupancy_norm = TRUE) +
  ggokabeito::scale_color_okabe_ito(order = okabe_order) +
  ggokabeito::scale_fill_okabe_ito(order = okabe_order)


## ----moving_average_1min_data-------------------------------------------------
# R code

activity_1min <- activity_1min |>
  dplyr::arrange(cage_name) |>
  dplyr::group_by(cage_name) |>
  dplyr::mutate(activity_ma = moving_average(movement_mean_per_cage_cm_s_min, n = 15)) |>
  dplyr::ungroup()

glimpse(activity_1min)


## ----plot_spaghetti_1min_ma---------------------------------------------------
# R code

spaghetti_plot(activity_data = activity_1min,
               metadata = metadata,
               yvar = "activity_ma",
               occupancy_norm = TRUE) +
  ggokabeito::scale_color_okabe_ito(order = okabe_order)


## ----plot_ribbon_1min_ma------------------------------------------------------
# R code

group_mean_sem_ribbon_plot(activity_data = activity_1min,
                           metadata = metadata,
                           yvar = "activity_ma",
                           occupancy_norm = TRUE) +
  ggokabeito::scale_color_okabe_ito(order = okabe_order) +
  ggokabeito::scale_fill_okabe_ito(order = okabe_order)


## ----plot_ribbon_1min_ma_zoomed-----------------------------------------------
# R code

group_mean_sem_ribbon_plot(activity_data = activity_1min,
                           metadata = metadata,
                           yvar = "activity_ma",
                           occupancy_norm = TRUE,
                           xlim = xlimits_firstdose) +
  ggokabeito::scale_color_okabe_ito(order = okabe_order) +
  ggokabeito::scale_fill_okabe_ito(order = okabe_order)


## ----add_dose_to_1min---------------------------------------------------------
# R code

activity_1min_joined <- activity_1min |>
  dplyr::left_join(annotation_group, by = "cage_name")


## ----summarize_4hour_timebins_after_dose1-------------------------------------
# R code

activity_1min_summarize <- activity_1min_joined |>
  dplyr::filter((start >= dose1_time & start <= (dose1_time + 60 * 60 * 4)) |
                  (start >= dose1_time - (60 * 60 * 24) & start <= dose1_time - (60 * 60 * 20))) |>
  dplyr::group_by(group_name, cage_name, start_date_local) |>
  dplyr::mutate(activity_occupancynorm = movement_mean_per_cage_cm_s_min / animals_cage_quantity) |>
  dplyr::summarize(postdose_0to4hr = mean(activity_occupancynorm, na.rm = TRUE)) |>
  dplyr::ungroup() |> 
  dplyr::group_by(group_name, cage_name) |>
  dplyr::arrange(start_date_local) |>
  dplyr::summarize(baseline_subtract_postdose_0to4hr = postdose_0to4hr[2] - postdose_0to4hr[1]) |>
  dplyr::ungroup()

activity_1min_summarize


## ----beeswarm_baseline_corrected----------------------------------------------
# R code

# Getting the ggplot add-on packages
library("ggbeeswarm")
library("ggrepel")

ggplot2::ggplot(data = activity_1min_summarize,
                aes(x = group_name, y = baseline_subtract_postdose_0to4hr,
                     color = group_name, label = cage_name)) +
  ggbeeswarm::geom_beeswarm() +
  ggrepel::geom_label_repel() +
  ggokabeito::scale_color_okabe_ito(order = okabe_order) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                 legend.position = "none") +
  ggplot2::xlab(NULL) +
  ggplot2::ylab("Baseline-Corrected Average Activity:\n4 Hours after Dose 1 (cm/s)")


## ----ttest--------------------------------------------------------------------
# R code

activity_4hr_post_dose_ttest <- t.test(baseline_subtract_postdose_0to4hr ~ group_name, 
                                     data = activity_1min_summarize)
activity_4hr_post_dose_ttest


## ----dark_period_analysis-----------------------------------------------------
# R code

activity_1min_summarize_darkdose <- activity_1min_joined |>
  dplyr::filter((start >= dose2_time & start <= (dose2_time + 60 * 60 * 4)) |
                  (start >= dose2_time - (60 * 60 * 24) & start <= dose2_time - (60 * 60 * 20))) |>
  dplyr::group_by(group_name, cage_name, start_date_local) |>
  dplyr::mutate(activity_occupancynorm = movement_mean_per_cage_cm_s_min / animals_cage_quantity) |>
  dplyr::summarize(postdose_0to4hr = mean(activity_occupancynorm, na.rm = TRUE)) |>
  dplyr::ungroup() |> 
  dplyr::group_by(group_name, cage_name) |>
  dplyr::arrange(start_date_local) |>
  dplyr::summarize(baseline_subtract_postdose_0to4hr = postdose_0to4hr[2] - postdose_0to4hr[1]) |>
  dplyr::ungroup()

ggplot2::ggplot(data = activity_1min_summarize_darkdose,
                aes(x = group_name, y = baseline_subtract_postdose_0to4hr,
                     color = group_name, label = cage_name)) +
  ggbeeswarm::geom_beeswarm() +
  ggrepel::geom_label_repel() +
  ggokabeito::scale_color_okabe_ito(order = okabe_order) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                 legend.position = "none") +
  ggplot2::xlab(NULL) +
  ggplot2::ylab("Baseline-Corrected Average Activity:\n4 Hours after Dose 2 (cm/s)")


## ----darkdose_ttest-----------------------------------------------------------
# R code

activity_4hr_post_darkdose_ttest <- t.test(baseline_subtract_postdose_0to4hr ~ group_name, 
                                           data = activity_1min_summarize_darkdose)
activity_4hr_post_darkdose_ttest


## ----datetime_minute----------------------------------------------------------
# R code

# Producing an arbitrary hour
datetime_minute <- as.POSIXct("2024-06-26 06:12:00", tz = "UTC")

# Showing this minute in numeric format
as.numeric(datetime_minute)


## ----modulo_hour--------------------------------------------------------------
# R code

# Getting the hour in which this date and time starts
# There are 3600 seconds in an hour
# Subtracting the modulo of the UNIX time stamp divided by 3600
datetime_inhour <- datetime_minute - as.numeric(datetime_minute) %% 3600

# Showing the hour interval for the datetime
datetime_inhour


## ----encode_animal_age--------------------------------------------------------
# R code

# Getting a vector of presumed dates of birth
dob = as.POSIXct(c("2024-01-20","2024-01-22"))

# Getting a vector of a date
today = as.POSIXct(c("2024-06-24","2024-06-24"))

# Computing days postnatal
days_postnatal = reltime(rawtimes = today, 
                         reftimes = dob,
                         units = "days")

# Displaying days postnatal
days_postnatal


## ----weeks_postnatal----------------------------------------------------------
# R code

# Getting days at which animals were a certain age postnatal.
weeks_postnatal = reltime(rawtimes = today,  
                          reftimes = dob,
                          units = "weeks")

# Displaying days postnatal
weeks_postnatal


## ----weeks_postnatal_seq_function---------------------------------------------
# R code

# Making a weeks postnatal sequence function
first_16 <- weeks_postnatal_seq(dob = as.Date("2024-01-20"), 
                                n_weeks = 16)
first_16


## ----get_utc_offset-----------------------------------------------------------
# R code


# Making arbitrary timestamps that have different UTC offsets
ts <- as.POSIXct(c("2024-02-28 00:00:00",
                   "2024-03-28 00:00:00"),
                 tz = "US/Eastern")

# Displaying offsets
get_utc_offset(ts, as_numeric = TRUE)


## ----sessioninfo--------------------------------------------------------------
# R code

sessionInfo()

