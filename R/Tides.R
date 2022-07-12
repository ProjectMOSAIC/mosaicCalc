#' Tide levels from the US NOAA
#'
#' - `RI_tide`: For Providence, RI, Minute-by-minute tide data during April 1-5 2010.
#' - `Anchorage_tide`: For Anchorage, AK, Every six-minute data during 2018
#'
#' Variables:
#' - `level` meters
#' - `date_time` GMT
#' - `hour` time in hours after start of April 2010.
#'
#' Lat/Long: 41.8071N, -71.4012W
#' Request: `https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20200401%2010:00&end_date=20200405%201:59&station=8454000&product=one_minute_water_level&datum=mllw&units=metric&time_zone=gmt&application=web_services&format=csv`
#' Other product documentation: https://tidesandcurrents.noaa.gov/api/

#' Google Map: https://www.google.com/maps/place/Providence,+RI/@41.8071,-71.4012,14z/data=!4m5!3m4!1s0x89e444e0437e735d:0x69df7c4d48b3b627!8m2!3d41.8071!4d-71.4012
#' @rdname Tides
#' @aliases RI_tide, Anchorage_tide
"RI_tide"
