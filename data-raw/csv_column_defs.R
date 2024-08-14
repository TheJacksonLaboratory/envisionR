csv_column_defs <- list(
  version_numbers = c("v0.0.0.9000", "vTEST"),
  demographics = list("v0.0.0.9000" = list(
    Group = readr::col_character(),
    Cage = readr::col_character(),
    `Animal ID` = readr::col_character(),
    `Murine ear tag` = readr::col_character(),
    Strain = readr::col_character(),
    `Coat Color` = readr::col_character(),
    Genotype = readr::col_character(),
    `Additional detail` = readr::col_character(),
    Sex = readr::col_character(),
    `Birth date` = readr::col_datetime(),
    `Death date` = readr::col_datetime(),
    `Ear notch` = readr::col_character(),
    `Metal ear tag` = readr::col_character(),
    `Other ID` = readr::col_character(),
    `RapID tag code` = readr::col_character(),
    `RapID tag color` = readr::col_character(),
    RFID = readr::col_character(),
    `Tail tattoo` = readr::col_character()
  )),
  cage_activity_hour = list("v0.0.0.9000" = list(
    start = readr::col_datetime(format = ""),
    start.date.local = readr::col_date(format = ""),
    start.time.local = readr::col_time(format = ""),
    study.code = readr::col_character(),
    aggregation.seconds = readr::col_double(),
    group.name = readr::col_character(),
    cage.name = readr::col_character(),
    animals.cage.quantity = readr::col_integer(),
    light.cycle = readr::col_character(),
    movement.mean.per_cage.cm_s.hour = readr::col_double(),
    wheel_occupancy.mean.per_cage.s.hour = readr::col_double(),
    food_occupancy.mean.per_cage.s.hour = readr::col_double(),
    water_occupancy.mean.per_cage.s.hour = readr::col_double()
  )),
  animal_activity_hour = list("v0.0.0.9000" = list(
    start = readr::col_datetime(format = ""),
    start.date.local = readr::col_date(format = ""),
    start.time.local = readr::col_time(format = ""),
    study.code = readr::col_character(),
    aggregation.seconds = readr::col_double(),
    group.name = readr::col_character(),
    cage.name = readr::col_character(),
    animals.cage.quantity = readr::col_integer(),
    light.cycle = readr::col_character(),
    animal.id = readr::col_character(),
    strain = readr::col_character(),
    sex = readr::col_character(),
    genotype = readr::col_character(),
    birth.date = readr::col_date(format = ""),
    movement.animal.cm_s.hour = readr::col_double(),
    wheel_occupancy.animal.s.hour = readr::col_double(),
    food_occupancy.animal.s.hour = readr::col_double(),
    water_occupancy.animal.s.hour = readr::col_double()
  )),
  cage_activity_10min = list("v0.0.0.9000" = list(
    start = readr::col_datetime(format = ""),
    start.date.local = readr::col_date(format = ""),
    start.time.local = readr::col_time(format = ""),
    study.code = readr::col_character(),
    aggregation.seconds = readr::col_double(),
    group.name = readr::col_character(),
    cage.name = readr::col_character(),
    animals.cage.quantity = readr::col_integer(),
    light.cycle = readr::col_character(),
    movement.mean.per_cage.cm_s.10min = readr::col_double(),
    wheel_occupancy.mean.per_cage.s.10min = readr::col_double(),
    food_occupancy.mean.per_cage.s.10min = readr::col_double(),
    water_occupancy.mean.per_cage.s.10min = readr::col_double()
  )),
  animal_activity_10min = list("v0.0.0.9000" = list(
    start = readr::col_datetime(format = ""),
    start.date.local = readr::col_date(format = ""),
    start.time.local = readr::col_time(format = ""),
    study.code = readr::col_character(),
    aggregation.seconds = readr::col_double(),
    group.name = readr::col_character(),
    cage.name = readr::col_character(),
    animals.cage.quantity = readr::col_integer(),
    light.cycle = readr::col_character(),
    animal.id = readr::col_character(),
    strain = readr::col_character(),
    sex = readr::col_character(),
    genotype = readr::col_character(),
    birth.date = readr::col_date(format = ""),
    movement.animal.cm_s.10min = readr::col_double(),
    wheel_occupancy.animal.s.10min = readr::col_double(),
    food_occupancy.animal.s.10min = readr::col_double(),
    water_occupancy.animal.s.10min = readr::col_double()
  )),
  cage_activity_min = list("v0.0.0.9000" = list(
    start = readr::col_datetime(format = ""),
    start.date.local = readr::col_date(format = ""),
    start.time.local = readr::col_time(format = ""),
    study.code = readr::col_character(),
    aggregation.seconds = readr::col_double(),
    group.name = readr::col_character(),
    cage.name = readr::col_character(),
    animals.cage.quantity = readr::col_integer(),
    light.cycle = readr::col_character(),
    movement.mean.per_cage.cm_s.min = readr::col_double(),
    wheel_occupancy.mean.per_cage.s.min = readr::col_double(),
    food_occupancy.mean.per_cage.s.min = readr::col_double(),
    water_occupancy.mean.per_cage.s.min = readr::col_double()
  )),
  animal_activity_min = list("v0.0.0.9000" = list(
    start = readr::col_datetime(format = ""),
    start.date.local = readr::col_date(format = ""),
    start.time.local = readr::col_time(format = ""),
    study.code = readr::col_character(),
    aggregation.seconds = readr::col_double(),
    group.name = readr::col_character(),
    cage.name = readr::col_character(),
    animals.cage.quantity = readr::col_integer(),
    light.cycle = readr::col_character(),
    animal.id = readr::col_character(),
    strain = readr::col_character(),
    sex = readr::col_character(),
    genotype = readr::col_character(),
    birth.date = readr::col_date(format = ""),
    movement.animal.cm_s.min = readr::col_double(),
    wheel_occupancy.animal.s.min = readr::col_double(),
    food_occupancy.animal.s.min = readr::col_double(),
    water_occupancy.animal.s.min = readr::col_double()
  )),
  annotation = list("v0.0.0.9000" = list(
    ID = readr::col_double(),
    created = readr::col_datetime(format = ""),
    created_date.local = readr::col_date(format = ""),
    created_time.local = readr::col_time(format = ""),
    pin_start_date.local = readr::col_date(format = ""),
    pin_start_time.local = readr::col_time(format = ""),
    pin_end_date.local = readr::col_date(format = ""),
    pin_end_time.local = readr::col_time(format = ""),
    study_code = readr::col_character(),
    group_name = readr::col_character(),
    cage_name = readr::col_character(),
    creator = readr::col_character(),
    contents = readr::col_character(),
    reply_to = readr::col_character(),
    hashtags = readr::col_character()
  ))
)

csv_column_defs[["demographics"]][["vTEST"]] <- csv_column_defs[["demographics"]][["v0.0.0.9000"]]

csv_column_defs[["cage_activity_hour"]][["vTEST"]] <- csv_column_defs[["cage_activity_hour"]][["v0.0.0.9000"]]
csv_column_defs[["animal_activity_hour"]][["vTEST"]] <- csv_column_defs[["animal_activity_hour"]][["v0.0.0.9000"]]

csv_column_defs[["cage_activity_10min"]][["vTEST"]] <- csv_column_defs[["cage_activity_10min"]][["v0.0.0.9000"]]
csv_column_defs[["animal_activity_10min"]][["vTEST"]] <- csv_column_defs[["animal_activity_10min"]][["v0.0.0.9000"]]

csv_column_defs[["cage_activity_min"]][["vTEST"]] <- csv_column_defs[["cage_activity_min"]][["v0.0.0.9000"]]
csv_column_defs[["animal_activity_min"]][["vTEST"]] <- csv_column_defs[["animal_activity_min"]][["v0.0.0.9000"]]

csv_column_defs[["annotation"]][["vTEST"]] <- csv_column_defs[["annotation"]][["v0.0.0.9000"]]

usethis::use_data(csv_column_defs, internal = FALSE, overwrite = TRUE)
