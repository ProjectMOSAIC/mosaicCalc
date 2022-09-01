#' Dynamical systems calculations and graphics
#'
#' - `streamlines()` draws raindrop-shaped paths at a randomized grid of points that follow trajectories
#' - `flow_field()` draws arrows showing the flow at a grid of points
#' - `trajectory_euler()` compute an Euler solution. ()
#'
#' @param \dots The first arguments should describe the dynamics. See details.
#' @param scale Number indicating how long to draw arrows. By default,
#' the longest arrows take up a full box in the grid
#' @param npts The number of points on an edge of the grid
#' @param dt Time step for integrating the streamlines.
#' @param nsteps How many steps to take for each streamline. Together with `dt`
#' this determines the length of the streamline.
#' @param color What color to use
#' @param alpha What alpha to use
#'
#'
#' @details The dynamical functions themselves will be formulas like `dx ~ a*x*y` and `dy ~ y/x`.
#' Initial conditions will be arguments of the form `x=3` and `y=4`.
#' If there are parameters in the dynamical functions, you should also
#' add the parameter values, for instance `a=2`.
#'
#' For `flow_field()` and `streamlines()` you do not need to specify
#' initial conditions. The grid will be set by the domain argument.
#'
#' The graphics functions are all arranged to accept,
#' if given, a ggplot object piped in. The new graphics layer will be drawn on
#' top of that. If there is no ggplot object piped in, then the graphics
#' will be made as a first layer, which can optionally piped into other
#' ggformula functions or `+` into ggplot layers.
#'
#' @examples
#' streamlines(dx ~ x+y, dy~ x-y, domain(x=0:6, y=0:3))
#' flow_field(dx ~ x+y, dy~ x-y, domain(x=0:6, y=0:3))
#' Dyn <- makeODE(dx ~ 0.06*x, dy ~-x - y, domain(t=0:20), dt=0.1,x=3, y=4)
#' streamlines(Dyn, domain(x=-5:5, y=-5:5), npts=15)
#' flow_field(Dyn, domain(x=-6:6, y=-10:10))
#' @rdname dynamics
#' @export
streamlines <- function(..., npts=8, dt=0.01, nsteps=10, color="black", alpha=1) {
  Dyn <- makeODE(...)
  dom <- Dyn$domain  # Graphical domain
  # check if the domain is in the ... arguments. 
  if (length(dom) < 2) stop("domain must have two variables")
  xpts <- seq(min(dom[[1]]), max(dom[[1]]), length = npts)
  ypts <- seq(min(dom[[2]]), max(dom[[2]]), length = npts)
  dx <- diff(xpts[1:2])
  dy <- diff(ypts[1:2])
  grid <- as.matrix(expand.grid(list(xpts, ypts)))
  grid[,1] <-grid[,1] + runif(nrow(grid),min=-dx, max=dx)/2
  grid[,2] <-grid[,2] + runif(nrow(grid),min=-dy, max=dy)/2

  Flows <- list()
  for (point in 1:nrow(grid)) {
    x <- y <- numeric(nsteps)
    x[1] <- grid[point,1]
    y[1] <- grid[point,2]

    for (k in 2:length(x)) {
      # Euler integration 
      step = dt*Dyn$vfun(c(x[k-1], y[k-1]))
      x[k] <- x[k-1] + step[1]
      y[k] <- y[k-1] + step[2]
    }

    Flows[[point]] <- tibble::tibble(x=x, y=y,
                                     group=point,
                                     alpha = 0.2+(1:nsteps)/(1.2*nsteps),
                                     size = alpha + 0.6
    )
  }
  Paths <- dplyr::bind_rows(Flows)
  # get the last point in the path
  Last <- Paths %>% group_by(.data$group) %>%
    filter(row_number() == n())

  P <- Dyn$gg
  if (length(P) < 2) P <- NULL # no gg object was piped in
  P %>% ggformula::gf_path(y ~ x, data = Paths,
                     lineend = "round",
                     group = ~ group, color=color, alpha=alpha, size= ~size, inherit=FALSE) %>%
    #ggformula::gf_point(y ~ x, data = Last, shape=23, fill="black", inherit=FALSE) %>%
    ggformula::gf_labs(x = names(dom)[1], y = names(dom)[2]) %>%
    gf_refine(scale_alpha_identity(),
              scale_size_identity())
}

#' @rdname dynamics
#' @export
flow_field <- function(..., npts=8, scale=0.8, color="black", alpha=1) {
  Dyn <- makeODE(...)
  dom <- Dyn$domain  # Graphical domain
  # check if the domain is in the ... arguments. 
  if (length(dom) < 2) stop("domain must have two variables")
  #dyn <- parse_dynamics(first,..., req_initials=FALSE)
  if (length(dom) != 2) stop("domain must have two variables")
  xpts <- seq(min(dom[[1]]), max(dom[[1]]), length = npts)
  ypts <- seq(min(dom[[2]]), max(dom[[2]]), length = npts)
  grid <- as.matrix(expand.grid(list(xpts, ypts)))

  steps <- t(apply(grid, 1, FUN=Dyn$vfun))
  step_lengths <- sqrt(steps[,1]^2 + steps[,2]^2)
  dx <- diff(xpts[1:2])
  dy <- diff(ypts[1:2])
  # add two columns to grid
  grid <- cbind(grid, grid)
  scale <- scale / max(step_lengths)
  xstep <- scale*steps[,1]*dx
  ystep <- scale*steps[,2]*dy
  grid[,1] <- grid[,1] - xstep
  grid[,2] <- grid[,2] - ystep
  grid[,3] <- grid[,3] + xstep
  grid[,4] <- grid[,4] + ystep

  res <- as.data.frame(na.omit(grid))
  names(res) <- c("x", "y", "xend", "yend")

  Dyn$Pprev %>% # previous graphics layer, if any
    gf_segment(y + yend ~ x + xend, data=res,
             color=color, alpha=alpha,
             arrow = arrow(ends="last", type="closed", length=unit(1, "mm")),
             inherit=FALSE)
}
#'

parse_dynamics <- function(first, ..., req_initials=TRUE) {
  # Grab all arguments, unevaluated
  # <first> is to make sure there is an argument to pipe into
  res <- list()
  res$gg <- if (inherits(first, "gg")) first
  else NA

  if (inherits(first, "formula")) {
    args <- c(first, enquos(...))
  } else {
    args <- enquos(...)
  }
  argnames <- names(args)
  if (length(args) == 0)
    stop("No arguments describing dynamics given.")


  f_names <- c()
  f_vars  <- c()
  dyn_args <- c()
  params <- c()
  others <- list()

  # see if the first one is a ggplot object
  first <- eval(rlang::get_expr(args[[1]]))


  # sort out which ones are dynamics formula style and which are
  # initial conditions
  for (k in 1:length(args)) {
    this <- eval(rlang::get_expr(args[[k]]))
    if (inherits(this, "formula")) {
      f_names <- union(f_names, all.vars(rlang::f_lhs(this)))
      f_vars  <- union(f_vars,  all.vars(rlang::f_rhs(this)))
      dyn_args <- c(dyn_args, k)
    } else if (inherits(this, "numeric")) {
      # it must be a parameter or an initial condition
      if (is.null(argnames[k])) {
        stop("Unnamed non-formula argument for dynamics. Parameters and initial conditions must be named.")
      } else {
        params[argnames[k]] <- this
      }
    } else {
      if (is.null(argnames[k]))
        stop("Unnamed non-parameter argument.")
      others[[argnames]] <- this
    }
  }

  # check names of formulas to be sure they appear in the list
  # of variables
  # strip out leading "d"
  res$state_names <- f_names <- gsub("^d(.{1})", "\\1", f_names )
  matches <- f_names %in% f_vars
  if (!all(matches)) stop("LHS for each formula needs to be one of the variables on the RHS.")
  # any of the parameters that match f_names are really an initial condition
  matches <- f_names %in% names(params)
  if (req_initials && !all(matches)) stop("Initial condition must be specified for each dynamical variable.")
  initials <- params[names(params) %in% f_names]
  params <- params[!names(params) %in% f_names]

  ## Construct the dynamical function with a vector input and t ???
  split_vec <-
    paste0(paste0(f_names,
           "<- vec[",
           1:length(f_names),
           "]", collapse="; "), ";")

  param_assign <- ""
  if (length(params) > 0) {
    param_assign <- paste(names(params), "<-", params, collapse="; ")
    param_assign <- paste0(param_assign, "; ")
  }


  component_expressions <- lapply(args[dyn_args],
                  FUN=function(ex)
                    deparse(rlang::f_rhs(
                      # rlang::get_expr(ex)))) %>% unlist()
                      rlang::eval_tidy(ex)))) %>% unlist()
  inside <- paste("c(",
                      paste(component_expressions, collapse=", "),
                      ")")
  res$params <- params
  res$initials <- initials

  body <- paste("{", param_assign, split_vec, inside, "}")

  res$dyn_fun <- function(vec){}
  body(res$dyn_fun) <- parse(text = body)


  res
}
#' @param dt time step (e.g. 0.01)
#' @param nsteps how many Euler steps to take
#' @param full report the derivative and the step size for each variable
#' @param every n, report will contain every nth step
#' @rdname dynamics
#' @examples
#' trajectory_euler(dx ~ -y, dy ~ .5*x, x=3, y=3)
#' rabbits <- drabbit ~ 0.2*rabbit - 0.01*rabbit*fox
#' foxes <- dfox ~ -.2*fox + 0.0005*rabbit*fox 
#' trajectory_euler(rabbits, foxes, rabbit=100, fox=3, dt=0.1, nsteps=500, every=10)
#'
#' @export
trajectory_euler <- function( ...,
                             dt=0.01, nsteps=4, full=TRUE, every=1) {
  Dyn <- makeODE(...)
  nsteps <- nsteps + 1 # Don't treat initial condition as a step
  dt <- Dyn$dt
  Step <- DState <- State <- matrix(0, ncol=length(Dyn$names), nrow=nsteps)
  colnames(State) <- Dyn$names
  colnames(DState) <- paste0("dt_", Dyn$names)
  colnames(Step) <- paste0("step_", Dyn$names)
  init_values <- Dyn$values[Dyn$names]
  if (length(init_values) != length(Dyn$names)) 
    stop("Must specify initial conditions")
  State[1,] <- unlist(init_values)
  for (k in 1:nsteps) {
    DState[k,] <- unlist(eval(Dyn$functions, envir=as.list(State[k,])))
    Step[k,] <- dt*DState[k,]
    if (k < nsteps) State[k+1,] <- State[k,] + Step[k,]
  }

  time <- tibble::tibble(t = dt*seq(0, nsteps-1))

  # trim the output if required.
  skip <- every - 1
  if (skip > 0) {
    if (skip != round(skip))
              stop("skip= must be a positive integer.")
    keepers <- seq(1, nsteps-1, by=skip+1L)
    time <- time[keepers,]
    State <- State[keepers,]
    DState <- DState[keepers,]
    Step <- Step[keepers, ]
  }

  # return(State)
  Res <- bind_cols(time, as.data.frame(State))

  if (full) Res <- bind_cols(Res,
                             as.data.frame(DState),
                             as.data.frame(Step))
  Res
}

