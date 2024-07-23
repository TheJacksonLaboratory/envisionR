# Getting lines for dummy CSV file
csv_lines_raw = "ID,created,created_date.local,created_time.local,pin_start_date.local,pin_start_time.local,pin_end_date.local,pin_end_time.local,study_code,group_name,cage_name,creator,contents,reply_to,hashtags
348,2024-01-20 17:48:24.338904+00:00,2024-01-20,09:48:24.338904,2024-01-19,18:18:02,,,STUDY1,G1,C1,John Smith,contents1,,
349,2024-01-21 19:17:03.248881+00:00,2024-01-21,11:17:03.248881,2024-01-20,08:58:44,,,STUDY1,G1,C3,Jane Doe,contents2,,
351,2024-01-22 09:04:24.439861+00:00,2024-01-22,01:04:24.439861,2024-01-21,10:10:10,,,STUDY1,G2,C4,Jane Doe,contents3,,
352,2024-01-22 21:01:01.010109+00:00,2024-01-22,13:01:01.010109,2024-01-21,11:11:11,,,STUDY1,G1,C1,Bot Bot,contents4,,"

# Getting lines for dummy CSV file
annotation_csv_example = unlist(strsplit(csv_lines_raw, "\n"))

usethis::use_data(annotation_csv_example, internal = FALSE, overwrite = TRUE)
