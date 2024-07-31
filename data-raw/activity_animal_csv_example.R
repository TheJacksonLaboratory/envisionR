csv_lines_raw <- "start,start.date.local,start.time.local,study.code,aggregation.seconds,group.name,cage.name,animals.cage.quantity,light.cycle,animal.id,strain,sex,genotype,birth.date,movement.animal.cm_s.hour,wheel_occupancy.animal.s.hour,food_occupancy.animal.s.hour,water_occupancy.animal.s.hour
2023-11-01 23:00:00+00:00,2023-11-01,16:00:00,test1,3600,G1,A1,3,Light,A1,C57BL/6J,male,WT,2024-01-03,0.5,0,0.003,0
2023-11-02 00:00:00+00:00,2023-11-01,17:00:00,test1,3600,G1,A1,3,Light,A2,A/J,female,KO,2023-12-24,1.2,0.025,0.004,0.0002
2023-11-02 01:00:00+00:00,2023-11-01,18:00:00,test1,3600,G1,A1,3,Dark,A3,DBA/2J,female,KO,2024-01-10,5,0.1,0.07,0.012
2023-11-02 02:00:00+00:00,2023-11-01,19:00:00,test1,3600,G1,A1,3,Dark,A4,WSB/EiJ,male,flox,2023-12-31,4.5,0.08,0.1,0.008"

# Getting lines for dummy CSV file
activity_animal_csv_example <- unlist(strsplit(csv_lines_raw, "\n"))

usethis::use_data(activity_animal_csv_example, internal = FALSE, overwrite = TRUE)
