#' NASA data on planets
#'

#'
#'
#' @format A data frame with 10 rows and these variables:
#'
#' - `planet` name of the planet
#' - `mass`  in 10^24 kg
#' - `diameter` in  km
#' - `density` in kg/m^3
#' - `gravity`  in m/s^2
#' - `escape_velocity` in km/s
#' - `rotational_period` in hours
#' - `day_length` in  hours
#' - `distance_from_sun` in 10^6 km
#' - `perihelion` in  10^6 km
#' - `aphelion` in  10^6  km
#' - `orbital_period` in days
#' - `orbital_velocity`  in km/s
#' - `orbital_inclination` in degrees
#' - `orbital_eccentricity`
#' - `obliquity_to_orbit` in  degrees
#' - `mean_temperature` in C
#' - `surface_pressure` in  bars
#' - `n_moons` a count
#' - `ring_system` does the planet have a ring system
#' - `global_magnetic_field` does the planet have a magnetic field

#'
#' @details Two of the rows, MOON and  PLUTO, are not considered planets. The
#' orbital parameters  of the MOON are with respect to EARTH. Others are with respect
#' to the Sun. Negative rotation period denotes a rotation opposite that of most of the
#' other planets.
#'
#' @docType data
#' @name Planets
#' @usage data(Planets)
#'
#' @keywords datasets
#'
#' @source https://nssdc.gsfc.nasa.gov/planetary/factsheet/
#'

#'
"Planets"
