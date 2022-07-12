# Make the Anchorage 2018 tide record

# Each month was gotten by a request such as this:
# https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?begin_date=20181201 10:00&end_date=20181231 10:24&station=9455920&product=water_level&datum=mllw&interval=h&units=metric&time_zone=gmt&application=web_services&format=csv
# There's a 31 day maximum

tide_names <- dir(path="~/Downloads/", pattern = "9455920", full.names=TRUE)
df_list <- lapply(tide_names, readr::read_csv)
All <- bind_rows(df_list) %>% arrange(`Date Time`)
All <- All %>% select(`Date Time`, `Water Level`)
names(All) <- c("date_time", "level")
All$hour <- with(All, (lubridate::yday(date_time)-1) * 24 + lubridate::hour(date_time) + lubridate::minute(date_time)/60)
Anchorage_tide <- All
save(Anchorage_tide, file = "data/Anchorage_tide.rda")
