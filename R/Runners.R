#' Running times
#'
#' A longitudinal record of running times. There are 5977 individual runners included
#' here, each with a unique `id`. Each runner ran the 10-mile race (See `mosaicData::TenMileRace`)
#' on multiple occasions.
#'
#' @format Data frame with 24,334 rows
#' - `year` the occasion on which the runner ran the Cherry Blossom Ten Miler
#' - `age` the runners age at the time of that occasion
#' - `gun` the time (in minutes) from the gun to crossing the finish line.
#' - `net` time between crossing the start line and crossing the finish line. Sometimes this is missing. It's typically smaller than `gun` because it takes time to reach the starting line.
#' - `sex` as recorded by the race organizers.
#' - `previous` How many previous occasions did this person run the race.
#' - `nruns` The total number of runs recorded for this person (including future runs)
#' - `start_position` A qualitative indication of how close to the front of the pack the person was positioned at the time the gun went off.
#' 
#' @source Scraped from the Cherry Blossom web site by patient, unremunerated toil of the author.
#' 
#' @author Daniel Kaplan
#' 
#' @docType data
"Runners"



