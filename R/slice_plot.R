#' Plot a function of a single variable
#'
#' In a slice plot, there  is one independent variable. The graph
#' shows the output of the function versus the independent variable. It's
#' called a slice plot to distinguish it from a contour plot, in which
#' the graph has one axis for each independent variable and the output
#' of the function is shown by color and labels.
#'
#' @param object (Not intended for user. Receives the input from  any previous graphics layers.)
#' @param formula A tilde formula with one independent variable. All parameters must
#' be assigned specific numerical values.
#' @param domain A named list giving the extent of the x-axis scale. If there
#' is a previous plot layer using the same independent variable name, the
#' domain can be omitted and will be inferred from the previous layer.
#' @param npts Integer, number of points at which to evaluate the function.
#' @param label_text character string label to place near the graph curve. Default: none.
#' @param label_x number between 0 and 1 indicating the horizontal placement  of the `label_text`.
#'
#' Additional arguments will be passed to `geom_line()`. Use, e.g. `color="red"`
#' @examples
#' \dontrun{
#' slice_plot(sin(x) ~ x, domain(x = range(-5, 15)))
#' f <- makeFun(sin(2*pi*t/P) ~ t)
#' slice_plot(f(t, P=20) ~ t, domain(t = range(-5, 10)), label_text = "Period 20", label_x=0.9)
#' }
#'
#' @export
slice_plot <- function(object, formula, domain, npts=100,
                       label_text =  "", label_x = 1, label_vjust="top", ...) {
  # deal with having to accept previous layers
  # or this being the first layer
  if (rlang::is_formula(object)) {
    if (!missing(formula)) domain <- formula
    formula <- object
    object <- NULL
    if (missing(domain) || is.null(domain))
      stop("Domain must be specified when there is no preceeding layer. ")
  } else if (inherits(object, "gg")) {
    # get domain from ggplot object, unless domain is already specified
    if (missing(domain)) {
      look_for <- all.vars(formula[[3]]) # the input variable name
      if (look_for %in% names(object$data)) {
        domain <- list()
        domain[[look_for]] <- range(object$data[[look_for]])
      } else {
        stop("Must specify domain or use same x variable as previous layer.")
      }

    }
  } else {
    stop("First argument (or pipe input) must be either a formula  or a ggplot layer.")
  }

  # Check that formula  is a function of one variable
  independent_vars <- all.vars(formula[[3]])
  if (length(independent_vars) != 1)
    stop("Formula must have only one var on RHS of tilde.")

  Eval_grid <- mosaicCalc:::eval_on_domain(formula, domain, n = npts)
  the_mapping <- aes(x = !!as.name(names(Eval_grid)[1]),
                     y = .output.)

  if (is.null(object)) object <- ggplot(Eval_grid, the_mapping)

  P <- object + geom_line(data = Eval_grid, the_mapping, ...)

  # put the label in  place
  if (label_text != "") {
    n <- nrow(Eval_grid)
    row_num <- pmax(1, pmin(n, round(n * label_x)))
    xpos <- Eval_grid[row_num,1]
    ypos <- Eval_grid$.output.[row_num]
    P <- P + geom_text(x = xpos, y = ypos, label=label_text,
                       vjust=label_vjust)
  }

  P
}
