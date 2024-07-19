# Compilie time zone internal data source
# Uses the precompiled list from the lutz package as the source

library(lutz)
library(dplyr)
library(usethis)

tz_raw <- lutz::tz_list()

# Getting assumptions of time zones for given UTC offsets
tz_assume <- structure(list(tz_name = c("Etc/GMT+12", "US/Samoa", "US/Hawaii",
                                        "Pacific/Marquesas", "US/Alaska", "US/Alaska", "US/Pacific",
                                        "US/Mountain", "US/Pacific", "US/Central", "US/Mountain", "US/Central",
                                        "US/Eastern", "Canada/Atlantic", "US/Eastern", "Canada/Newfoundland",
                                        "Brazil/East", "Canada/Atlantic", "Canada/Newfoundland", "America/Nuuk",
                                        "America/Miquelon", "Atlantic/Cape_Verde", "America/Nuuk", "UTC",
                                        "Europe/London", "Europe/Zurich", "Europe/Zurich", "Europe/Helsinki",
                                        "Europe/Istanbul", "Europe/Helsinki", "Asia/Tehran", "Asia/Dubai",
                                        "Asia/Kabul", "Asia/Karachi", "Asia/Kolkata", "Asia/Kathmandu",
                                        "Asia/Almaty", "Asia/Yangon", "Asia/Jakarta", "Australia/West",
                                        "Australia/Eucla", "Asia/Tokyo", "Australia/South", "Australia/ACT",
                                        "Australia/Lord_Howe", "Australia/South", "Pacific/Efate", "Australia/ACT",
                                        "Pacific/Norfolk", "NZ", "Pacific/Chatham", "Pacific/Apia", "NZ",
                                        "NZ-CHAT", "Pacific/Kiritimati"),
                            utc_offset_h = c(-12, -11,
                                             -10, -9.5, -9, -8, -8, -7, -7, -6, -6, -5, -5, -4, -4, -3.5,
                                             -3, -3, -2.5, -2, -2, -1, -1, 0, 1, 1, 2, 2, 3, 3, 3.5, 4, 4.5,
                                             5, 5.5, 5.75, 6, 6.5, 7, 8, 8.75, 9, 9.5, 10, 10.5, 10.5, 11,
                                             11, 12, 12, 12.75, 13, 13, 13.75, 14),
                            assume = c(1L, 1L, 1L,
                                       1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                       1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                       1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                       1L, 1L, 1L, 1L)), class = "data.frame", row.names = c(NA, -55L
                                       ))

# Getting overrides of time zones for given UTC offsets given any DST-related ambiguity
tz_override <- structure(list(tz_name = c("Etc/GMT+12", "US/Samoa", "US/Hawaii",
                                          "Pacific/Marquesas", "US/Alaska", "US/Pacific", "US/Pacific",
                                          "US/Central", "US/Eastern", "US/Eastern", "Canada/Newfoundland",
                                          "Brazil/East", "Canada/Newfoundland", "America/Nuuk", "Atlantic/Cape_Verde",
                                          "UTC", "Europe/Zurich", "Europe/Zurich", "Europe/Helsinki", "Asia/Tehran",
                                          "Asia/Dubai", "Asia/Kabul", "Asia/Karachi", "Asia/Kolkata", "Asia/Kathmandu",
                                          "Asia/Almaty", "Asia/Yangon", "Asia/Jakarta", "Australia/West",
                                          "Australia/Eucla", "Asia/Tokyo", "Australia/South", "Australia/ACT",
                                          "Australia/South", "Australia/ACT", "NZ", "Pacific/Chatham",
                                          "NZ", "NZ-CHAT", "Pacific/Kiritimati"),
                              utc_offset_h = c(-12,
                                               -11, -10, -9.5, -9, -8, -7, -6, -5, -4, -3.5, -3, -2.5, -2, -1,
                                               0, 1, 2, 3, 3.5, 4, 4.5, 5, 5.5, 5.75, 6, 6.5, 7, 8, 8.75, 9,
                                               9.5, 10, 10.5, 11, 12, 12.75, 13, 13.75, 14),
                              override = c(1L,
                                           1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                           1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                           1L, 1L, 1L, 1L, 1L, 1L, 1L)), class = "data.frame", row.names = c(NA,
                                                                                                             -40L))

timezones_df = tz_raw |>
  dplyr::left_join(tz_assume, by = c("tz_name", "utc_offset_h")) |>
  dplyr::left_join(tz_override, by = c("tz_name", "utc_offset_h")) |>
  dplyr::mutate(assume = ifelse(is.na(assume), 0, assume),
                override = ifelse(is.na(override), 0, override)) |>
  as.data.frame()

usethis::use_data(timezones_df, internal = TRUE, overwrite = TRUE)
