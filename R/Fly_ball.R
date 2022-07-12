#' Trajectory of a fly ball in baseball
#'
#' The trajectory of a fly ball as calculated by a sophisticated
#' model involving drag, spin, and such. The physics and mathematics are described
#' at <http://baseball.physics.illinois.edu/TrajectoryAnalysis.pdf>
#'
#' @details The ball has a mass of 5.125 ounces and a circumference of 9.125 inches
#' The speed off the bat was 103 mph at a launch angle of 27.5 degrees from
#' the horizontal. The ball was hit with backspin at 2500 rpm by a right-handed
#' batter. Ambient temperature 70 deg F with a relative humidity of 50% and a
#' barometric pressure of 29.92 inHg. The field was at an elevation of 15 feet.
#'
#' The position of the ball was recorded every 0.01 seconds, for 5.5 seconds altogether.
#' If you prefer to work with continuous functions of time, you
#' can construct them. See the examples.
#' 
#' @examples 
#' # Constructing continuous functions of time
#' ball_z <- mosaic::connector(z ~ t, data = Fly_ball)
#' ball_y <- mosaic::connector(y ~ t, data = Fly_ball)
#'
#' @source <http://baseball.physics.illinois.edu/TrajectoryCalculator-new-3D.xlsx>
#'
#' @format A data frame with 552 rows.
#'
#' - `t` - (seconds). Runs from 0 to 5.51 s
#' - `y` - horizontal position (feet)
#' - `z` - vertical position (feet)
#'
"Fly_ball"
