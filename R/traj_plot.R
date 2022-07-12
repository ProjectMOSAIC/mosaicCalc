#' Plots a trajectory 
#'
#' This function handles trajectories can stem from either of two sources:
#' 1. A parametric description of a curve, such as `sin(t) ~ cos(t)`, along with a 
#' domain in t.
#' 2. The solution to an ordinary differential equation as produce by `integrateODE()`
#'
#' @param \dots Handles the first several objects which are, in this order
#'     - tilde: a two sided tilde expression
#'     - soln: optionally, a solution object such as from `integrateODE()`, or instead
#'     - domain: a domain object, e.g. `domain(t=0:10)`
#'     
#'     
#' @param npts number of plotted points (default: 500)
#' @param nt number of tick marks to use in a trajectory plot
#' 
#' @details The tilde expression is the critical part
#' @examples
#' traj_plot(2*x + 3 ~ sin(x), domain(x=0:10))
#' PPdyn <- makeODE(dR ~  0.3*R - 0.03*R*F, dF ~ -0.3*F + 0.0003*R*F)
#' Soln <- integrateODE(PPdyn, domain(t=0:20), R=1200, F=8)
#' traj_plot(R(t) ~ F(t), Soln, nt=10)
#' traj_plot(R(t)*F(t) ~ t, Soln, nt=0)
#'
#' @export
traj_plot <- function(..., npts=500, nt=5) {
  dots <- list(...)
  
  # get the previous plot, if any
  if (inherits(dots[[1]], "gg")) {
    Pprev <- dots[[1]]
    dots[1] <- NULL
  } else Pprev <- NULL
  
  if (length(dots) < 2) stop("Must provide a tilde expression and a soln or domain")
  # get the tilde expression
  if (inherits(dots[[1]], "formula")) {
    tilde <- dots[[1]]
    dots[1] <- NULL
  }
  
  # Is this about the solution to an ODE?
  if (inherits(dots[[1]], "ODEsoln")) {
    soln <- dots[[1]]
    dots[1] <- NULL
  } else {
    soln <- NULL
  }
  # Is there a domain?
  if (length(dots) > 0 && inherits(dots[[1]], "xdomain")){
    dom <- dots[[1]]
    dots[1] <- NULL
  } else {
    if (is.null(soln)) stop("Must provide a solution from integrateODE, a domain, or both.")
    else {
    # Get one function from the <soln>. This is an interpolating function. The 
    # name of the input is, formally, "x" even though it stands for "t".
    # The result is to give the t-domain for the soln.
      lims <- range(environment(soln[[1]])$x)
      dom <- domain(t=range(lims))
    }
  }
  # process the formula
  # tilde expression should be two sided.
  if (length(tilde) != 3) stop("Two-sided tilde expression required.")
  
  DF <- eval_tilde_on_domain(tilde, dom, npts, dots, soln ) 
  
  
  tick_times <- tibble(x = pretty(range(DF[[1]]), n = nt))
  names(tick_times) <- names(DF)[1]

  Ticks <- eval_tilde_on_domain(tilde, tick_times, npts, dots, soln ) 
  Ticks$label <- tick_times[[1]]

  P <- Pprev %>% gf_path(.y ~ .x, data = DF, ...) %>%
    gf_labs(x = capture.output(tilde[[3]]), y=capture.output(tilde[[2]]))
  if (nt > 0) { # add tick marks
    P <- P %>%
      gf_label(.y ~ .x, data = Ticks, label=~label, vjust=1, hjust=1,...) %>%
      gf_point(.y ~ .x, data=Ticks, ...)

  }

  P
}

# Evaluate a one or two-sided for a range of inputs. 
eval_tilde_on_domain <- function(tilde, domain, npts, dots, soln=NULL) {
  if (inherits(domain, "xdomain")) {
    input_range <- range(domain[[1]])
    invals <- tibble(
      x = seq(input_range[1], input_range[2], length = npts)
    )
    names(invals) <- names(domain)[1]
  } else {
    # just numbers
    invals <- domain
  }
 
  # make a function from first expression in the tilde
  f2 <- function(){}
  body(f2) <- tilde[[2]]
  f2arg <- alist(x=)
  names(f2arg) <- names(invals)
  formals(f2) <- f2arg
  f2 <- f2 %>% bind_params(dots)
  
  if (length(tilde) == 2) {
    # add a dummy right-hand side to the tilde
    tilde[[3]] <- parse(text=names(invals))
  }
  
  if (!is.null(soln)) {
    nms <- names(soln)
    for (k in 1:length(soln)) assign(nms[k], soln[[k]]) 
  }
  
  f3 <- function(){}
  body(f3) <- tilde[[3]]
  f3arg <- alist(x=)
  names(f3arg) <- names(invals)
  formals(f3) <- f3arg
  f3 <- f3 %>% bind_params(dots)
  
  invals[[".y"]] <- f2(invals[[1]])
  invals[[".x"]] <- f3(invals[[1]])
  
  invals
} 

