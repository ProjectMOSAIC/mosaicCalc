#' Kepler's calculation of the position of Mars
#'
#' Astronomer [Johannes Kepler](https://en.wikipedia.org/wiki/Johannes_Kepler) (1571-1630) famously measured
#' the position of the planet Mars. His interpretation of this data
#' led substantially to Newton's theory of universal gravity. This
#' data frame gives Kepler's measurements as a function of time, together
#' with a modern reconstruction of the "actual" position of Mars at the time.
#'
#'
#' @format A data frame with 28 rows and these variables:
#'
#' - `time` - Interval in Earth days from 8:15am Greenwich
#' time on 9 March 1584.
#' - `kepler.radius` - The radius of the orbit in AU (astronomical units. 1 AU is 93 million miles)
#' - `kepler.angle` - The "true anomoly" measured in radians
#' - `actual.radius` - Modern calculation of the above
#' - `actual.angle` - Modern calculation of the above

#'
#' @details The raw measurements (not included here) that Kepler
#' used in his calculation were made by Tycho Brahe (1546-1601).
#' Those raw measurements were of the angle of Mars with respect to Earth.
#' Kepler estimated the orbital period of Mars to be 687 Earth days.
#' (The current accepted value is 686.980 days.) Knowing the period, Kepler
#' could find pairs of Earth days separated by multiples of the period. In each pair,
#' the Earth would be in a different position, but Mars would be in the same position.
#' Thus the distance of Mars from Earth could be estimated by triangulation.
#'
#' The angle was not directly measured for each occasion. Instead, knowing the
#' radius versus time Kepler was able to discern when Mars was at its greatest and
#' closest distance to the Sun. The angle tells where Mars is along its orbit. An
#' angle of 0 is the position when Mars is closest to the Sun. An angle of 3.14 is
#' when Mars is farthest from the Sun.
#'
#' @docType data
#' @name Kepler
#' @usage data(Kepler)
#'
#' @keywords datasets
#'
#' @source Drawn from  from McLaughlin, Michael P. ( 1999 ) "A Tutorial on
#' [Mathematical Modelling"](http://www.causascientia.org/math_stat/Tutorial.pdf)
#' p. 21-23.
#'
#' @references See `https://faculty.uca.edu/saustin/3110/mars.pdf` for a useful
#' description of the estimation process.
#'
"Kepler"
