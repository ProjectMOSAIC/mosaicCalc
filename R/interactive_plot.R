#' Make an interactive plotly plot of a function of two variables
#'
#' An interactive plot lets you interrogate the plot to get
#' numerical values at each point. When `type = "surface"`, the
#' plot can be rotated to see the "shape" of the function from
#' various perspectives. Using the  interactive controls, you
#' can save the plot as a PNG file. But it's not possible to overlay
#' plots the way you can with `contourPlot()`.
#'
#' @param formula a formula describing a function in  the manner
#' of `mosaicCalc::makeFun()`
#' @param domain  a call to the `domain()` function giving ranges
#' using the same independent variables as in the `formula`
#' @param npts The fineness at which to evaluate the function specified
#' by the formula in the  plot. Default: 50
#' @param type Plot type: `"surface"`,  `"contour"`, or `"heatmap"`
#'
#' @examples \dontrun{
#' interactive_plot(
#'     sin(fred*ginger) ~ fred + ginger,
#'     domain(fred=range(0,pi),
#'            ginger = range(0, pi)),
#'     type = "surface")
#' }
#'
#' @importFrom plotly plot_ly
#' @export
interactive_plot <- function(formula, domain,  npts=50,
                         type = "surface") {
  Eval_list <- mosaicUSAFA:::eval_as_vector_and_matrix(formula, domain, n = npts)
  plotly::plot_ly(x = Eval_list$x,
                  y = Eval_list$y,
                  z = Eval_list$.output.,
                  type = type)
}
