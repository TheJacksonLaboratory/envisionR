# Getting lines for dummy CSV file
csv_lines_raw = "Group,Cage,Animal ID,Murine ear tag,Strain,Coat Color,Genotype,Additional detail,Sex,Birth date,Death date,Ear notch,Metal ear tag,Other ID,RapID tag code,RapID tag color,RFID,Tail tattoo
G1,C1,1001,S4,C57BL/6J,black,WT,,male,2024-01-14,2024-05-10 18:35:00,,,,,black,,222
G1,C1,1002,S3,C57BL/6J,black,WT,,male,2024-01-12,2024-05-10 19:45:00,,,,,red,,111
G1,C1,1003,S2,C57BL/6J,black,WT,small,male,2024-01-13,2024-05-10 18:55:00,,,,,brown,,000
G2,C2,1004,S4,A/J,white,KO,,female,2024-01-21,2024-05-10 18:13:00,,,,,black,,222"

# Getting lines for dummy CSV file
demographics_csv_example = unlist(strsplit(csv_lines_raw, "\n"))

usethis::use_data(demographics_csv_example, internal = FALSE, overwrite = TRUE)
