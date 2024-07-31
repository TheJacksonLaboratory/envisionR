csv_lines_raw <- "start,start.date.local,start.time.local,study.code,aggregation.seconds,group.name,cage.name,animals.cage.quantity,light.cycle,movement.mean.per_cage.cm_s.hour,wheel_occupancy.mean.per_cage.s.hour,food_occupancy.mean.per_cage.s.hour,water_occupancy.mean.per_cage.s.hour
2023-11-01 23:00:00+00:00,2023-11-01,16:00:00,test1,3600,G1,A1,3,Light,2,0,0.003,0
2023-11-02 00:00:00+00:00,2023-11-01,17:00:00,test1,3600,G1,A1,3,Light,2.8,0.025,0.004,0.0002
2023-11-02 01:00:00+00:00,2023-11-01,18:00:00,test1,3600,G1,A1,3,Dark,6,0.1,0.07,0.012
2023-11-02 02:00:00+00:00,2023-11-01,19:00:00,test1,3600,G1,A1,3,Dark,5.5,0.08,0.1,0.008"

# Getting lines for dummy CSV file
activity_cage_csv_example <- unlist(strsplit(csv_lines_raw, "\n"))

usethis::use_data(activity_cage_csv_example, internal = FALSE, overwrite = TRUE)
