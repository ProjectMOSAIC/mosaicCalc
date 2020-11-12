#' Plot a vector field
#'
#' @export
gradient_plot <- function(object=NULL, formula, dom, npts=20, color="black", alpha=0.5) {
  if (rlang::is_formula(object)) {
    # shift first two arguments down to  become formula
    # and domain
    dom <- formula
    formula <-  object
    object <- NULL
  }
  dx_formula <- dy_formula <- formula
  dx_formula[[3]] <- formula[[3]][[2]]
  dy_formula[[3]] <- formula[[3]][[3]]
  dx <- D(dx_formula)
  dy <- D(dy_formula)

  vectorfield_plot(object,
                   dx(x=x, y=y) ~ x + y,
                   dy(x=x, y=y) ~ x + y,
                   dom = dom, npts = npts,
                   color=color, alpha=alpha)
}

#' @export
vectorfield_plot <- function(object=NULL, formula_x=NULL, formula_y=NULL,
                             dom, npts=20, color="black", alpha = 0.5) {
 if (rlang::is_formula(object)) {
    # shift first two arguments down to  become formula
    # and domain
    dom <- formula_y
    formula_y <-  formula_x
    formula_x <-  object
    object <- NULL

    if (missing(dom) || is.null(dom))
      stop("Domain must be specified when there is no preceeding layer.")
  }

  if (inherits(object, "ggplot")) {
    if (missing(dom)) {
      look_for <- all.vars(formula_x[[3]]) # the input variable names
      if (all(look_for %in% names(object$data))) {
        dom <- list()
        dom[[look_for[1]]] <- range(object$data[[look_for[1]]])
        dom[[look_for[2]]] <- range(object$data[[look_for[2]]])
      } else {
        stop("Must specify domain or use same x/y variables as previous layer.")
      }
    }
  }


  grid <- mosaicCalc:::eval_on_domain(formula_x, dom, n=npts)
  input_names <-  names(grid)
  input_names <- input_names[input_names != ".output."]
  grid$dx <- grid$.output.
  grid$dy <- mosaicCalc:::eval_on_domain(formula_y, dom, n=npts)$.output.
  # Now everything is in grid

  # Scale length as sqrt.
  length <- sqrt(sqrt(grid$dx^2 + grid$dy^2))
  angle <- atan2(grid$dy, grid$dx)
  longest <- max(length)
  length <- length / longest # scale to 1 for longest vector

  x_spacing <- diff(dom[[1]]) / npts
  y_spacing <- diff(dom[[2]]) / npts

  grid$x_end <- grid$x + x_spacing*length*cos(angle)
  grid$y_end <- grid$y + y_spacing*length*sin(angle)

  the_mapping <- aes(x = !!as.name(input_names[1]),
                     y = !!as.name(input_names[2]),
                     xend = x_end, yend = y_end)
  if (is.null(object))
    object <- ggplot(grid, the_mapping)

  P <- object

  P + geom_segment(data=grid, mapping=the_mapping,
                   arrow = arrow(length=unit(0.05, "inches")),
                   color = color, alpha = alpha)
}
