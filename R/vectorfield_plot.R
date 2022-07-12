#' Plot a vector field
#'
#' @param \dots (Optional: a previous graphics layer), formula for horizontal component, formula for vertical component,
#' (optionally: a domain)
#' @param npts number of arrows to draw in one row or column of the field.
#' @param color character string specifying the color of the arrows.
#' @param alpha transparency of the arrots
#' @param transform controls the relative length of the arrows. See details.
#'
#' @details Typically, the length of the arrows is not meaningful in the units of the
#' horizontal or vertical axis. For instance, in a gradient plot of f(x,y), the axis is in units of x, but
#' the gradient component has units of f(x,y)/x. Similarly for the flow of a differential
#' equation. Nonetheless, the relative lengths of the arrows, one to another, does have meaning.
#' In drawing the vector field, the arrows are scaled so that the longest one barely avoids contact with it's neighbors.
#' This natural scaling has the disadvantage that it can be hard to discern the lengths of the
#' shortest arrows, which often are near zero (as with the gradient near the argmax or argmin, or near
#' a fixed point of a differential equation flow field). By default, the relative lengths of the arrow
#' are transformed by sqrt, to make the long arrows shorter and therefore enable the sort arrows to
#' be drawn somewhat longer. If you want the natural scaling instead, use `transform=I`. Or you might
#' want to make the arrows even more similar in length. Then use, for instance `transform=function(L) L^0.1`
#'
#' @examples
#' gradient_plot(x * sin(y) ~ x & y, domain(x=-1:1, y=-1:1), transform=I)
#' vectorfield_plot(-y ~ x & y, x ~ x & y, domain(x=-1:1, y=-1:1))
#' gf_label(0 ~ 0, label="center", color="red") %>%
#' vectorfield_plot(-y ~ x & y, x ~ x & y, domain(x=-1:1, y=-1:1))
#' vectorfield_plot(-y ~ x & y, x ~ x & y, domain(x=-1:1, y=-1:1), transform=function(x) x^0.2 )
#' @export
gradient_plot <- function(..., # canonical first three arguments
                          #object=NULL, formula, domain,
                          npts=20, color="black", alpha=0.5,
                          transform=sqrt) {
  args <- first_three_args(...)
  object <- args$gg
  tilde <- args$tilde
  domain <- args$domain
  
  vnames <- all.vars(tilde[[3]])
  xn <- vnames[1]
  yn <- vnames[2]
  dx_tilde <- dy_tilde <- tilde
  dx_tilde[[3]] <- tilde[[3]][[2]]
  dy_tilde[[3]] <- tilde[[3]][[3]]
  dx <- D(dx_tilde) %>% bind_params(args$dots)
  dy <- D(dy_tilde) %>% bind_params(args$dots)
  still_unbound <- setdiff(
    c(unbound(dx), unbound(dy)), c(xn, yn))
  if (length(still_unbound) > 0) {
    msg <- paste0("Parameter", ifelse(length(still_unbound) > 1, 's ', ' '), 
                  paste0("<", still_unbound, ">", collapse=", "), 
                  " without specified numerical values.")
    stop(msg)
  }
  
  # Check that dx() and dy() are both functions of <xn> and <yn>
  missing_in_dx_or_dy <- c(setdiff(c(xn, yn), unbound(dx)), 
                     setdiff(c(xn,yn), unbound(dy)))
  if (length(missing_in_dx_or_dy) > 0) stop(
    paste("Domain variable", 
          paste0("<",missing_in_dx_or_dy,">", collapse=", "),
          "not used in LHS of tilde expression.")
  )
  
  
  # Make sure dx() and dy() are functions only of the domain names <xn> & <yn>
  horiz_formula <-
    as.formula(
      glue::glue("dx({xn}={xn}, {yn}={yn}) ~ {xn} + {yn}")
    )
  vert_formula <-
    as.formula(
      glue::glue("dy({xn}={xn}, {yn}={yn}) ~ {xn} + {yn}")
    )
  
  # check whether to inherit domain from previous layer
  if (inherits(object, "gg")) {
    if (is.null(domain)) {
      look_for <- all.vars(horiz_formula[[3]]) # the input variable names
      if (all(look_for %in% names(object$data))) {
        domain <- list()
        domain[[look_for[1]]] <- range(object$data[[look_for[1]]])
        domain[[look_for[2]]] <- range(object$data[[look_for[2]]])
      } else {
        stop("Must specify domain or use same x/y variables as previous layer.")
      }
    }
  }
  
  vectorfield_plot(object,
                   horiz_formula,
                   vert_formula,
                   domain = domain, 
                   npts = npts,
                   color=color, alpha=alpha,
                   transform=transform)
}
#' @rdname gradient_plot
#' @export
vectorfield_plot <- function(..., # canonical first four arguments
                             npts=20, color="black", alpha = 0.5,
                             transform = sqrt) {
  args <- first_three_args(..., two_tildes = TRUE)
  # gives $tilde and $tilde2
  object <- args$gg
  formula_x <- args$tilde
  formula_y <- args$tilde2
  domain <- args$domain
  

  
  # check whether to inherit domain from previous layer
  if (inherits(object, "gg")) {
    if (is.null(domain)) {
      look_for <- all.vars(formula_x[[3]]) # the input variable names
      if (all(look_for %in% names(object$data))) {
        domain <- list()
        domain[[look_for[1]]] <- range(object$data[[look_for[1]]])
        domain[[look_for[2]]] <- range(object$data[[look_for[2]]])
      } else {
        stop("Must specify domain or use same x/y variables as previous layer.")
      }
    }
  }

  # Bring in the dx() and dy() function definitions from the formulas
  # These will have been defined by gradient_plot() or will be null if 
  # vectorfield_plot was called directly.
  dx <- environment(formula_x)$dx
  dy <- environment(formula_y)$dy
  
  # Check that there are no unbound parameters in the two functions
  test_x <- makeFun(formula_x) %>% bind_params(args$dots)
  test_y <- makeFun(formula_y) %>% bind_params(args$dots)
  still_unbound <- setdiff(
    c(unbound(test_x), unbound(test_y)),
    names(domain)
  )
  if (length(still_unbound) > 0) {
    msg <- paste("Unbound parameters",
                 paste0("<", still_unbound, ">", collapse=", "),
                 "in one or both tilde expressions.")
    stop(msg)
  }
  
  grid <- eval_on_domain(formula_x, domain, n=npts, args$dots)
  input_names <-  names(grid)
  input_names <- input_names[input_names != ".output."]
  grid$dx <- grid$.output.
  grid$dy <- eval_on_domain(formula_y, domain, n=npts, args$dots)$.output.
  # Now everything is in grid

  # Scale length according to <transform> argument
  length <- transform(sqrt(grid$dx^2 + grid$dx^2))
  angle <- atan2(grid$dy, grid$dx)
  longest <- max(length)
  length <- length / longest # scale to 1 for longest vector

  x_spacing <- diff(domain[[1]]) / npts
  y_spacing <- diff(domain[[2]]) / npts

  # center arrow at gridpoint
  x_length <- x_spacing*length*cos(angle)/2 # half length
  y_length <- y_spacing*length*sin(angle)/2

  grid$x_start <- grid[[1]] - x_length
  grid$y_start <- grid[[2]] - y_length
  grid$x_end <- grid[[1]] + x_length
  grid$y_end <- grid[[2]] + y_length

  if (is.null(object))
    object <- ggplot(data=grid, 
                     aes(x = x_start,
                         y = y_start,
                         xend = x_end, yend = y_end))

  P <- object

  P + geom_segment(data=grid, 
                   aes(x = x_start,
                       y = y_start,
                       xend = x_end, yend = y_end),
                   arrow = arrow(length=unit(0.05, "inches")),
                   color = color, alpha = alpha) +
    labs(y = input_names[2], x = input_names[1])

}
