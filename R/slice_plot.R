#' Plot a function of a single variable
#'
#' In a slice plot, there  is one independent variable. The graph
#' shows the output of the function versus the independent variable. It's
#' called a slice plot to distinguish it from a contour plot, in which
#' the graph has one axis for each independent variable and the output
#' of the function is shown by color and labels.
#'
#' @param \dots Canonical first three argument: `[gg]`, tilde expression, `[domain]` as well
#' as any parameters to be assigned or re-assigned.
#' @param npts Integer, number of points at which to evaluate the function.
#' @param color Color of curve
#' @param alpha Alpha of curve
#' @param label_text character string label to place near the graph curve. Default: none.
#' @param label_x number between 0 and 1 indicating the horizontal placement  of the `label_text`.
#' @param label_vjust vertical justification of label. One of "left", "middle", "right", "bottom", "center", "top", "inward", or "outward"
#' @param label_color color of label
#' @param label_alpha alpha of label
#' @param singularities numeric vector of x positions at which to break
#' the graph.
#'
#' Additional arguments will be passed to `geom_line()`. Use, e.g. `color="red"`
#' @examples
#' \dontrun{
#' slice_plot(sin(x) ~ x, domain(x = range(-5, 15)))
#' f <- makeFun(sin(2*pi*t/P) ~ t)
#' slice_plot(f(t, P=20) ~ t, domain(t = -5:10), label_text = "Period 20", label_x=0.9)
#' slice_plot(x^2 ~ x) # Error: no domain specified
#' slice_plot(cos(x) ~ x, domain(x[-10:10]) # domain will be -10 < x < 10
#' # see domain
#' }
#'
#' @returns ggplot2 layers
#'
#' @export
slice_plot <- function(..., # canonical first three arguments
                       npts=101,
                       color="black",  alpha = 1,
                       label_text =  "", label_x = 1,
                       label_vjust="top",
                       label_color=color, label_alpha = alpha,
                       singularities = numeric(0)) {
  
  args <- first_three_args(...)
  object <- args$gg
  tilde <- args$tilde
  # Accepts a one-side tilde expression, turning it into a two-sided one.
  if (length(tilde)==2) tilde <- infer_RHS(tilde[[2]])
  domain <- args$domain
  if (!is.null(object)) {
    # get bounds from ggplot object, unless bounds is already specified
    if (is.null(domain)) {
      look_for <- all.vars(tilde[[3]]) # the input variable name
      if (look_for %in% names(object$data)) {
        domain <- list()
        domain[[look_for]] <- range(object$data[[look_for]])
      } else {
        stop("Must specify domain bounds or use same x variable as previous layer.")
      }
    }
  } 
  
  # make sure domain is specified
  if (is.null(domain)) {
    msg <- glue::glue("Must specify domain bounds after tilde expr, e.g. \n          slice_plot({capture.output(tilde)}, domain({all.vars(tilde[[3]])[1]}=0:10))")
    stop(msg)
  }

  # Check that tilde is a function of one variable
  independent_vars <- all.vars(tilde[[3]])
  if (length(independent_vars) != 1)
    stop("Tilde expression must have only one var on RHS.")
  
  # Check for unbound parameters
  testf <- makeFun(tilde) %>% bind_params(args$dots)
  unbound_args <- unbound(testf) %>% setdiff(independent_vars)
  if (length(unbound_args) > 0) {
    msg <- paste0("Parameter", ifelse(length(unbound_args) > 1, 's ', ' '), 
                  paste0("<", unbound_args, ">", collapse=", "), 
                 " without specified numerical values.")
    stop(msg)
  }
  
  # Fill in any missing parameters in tilde and evaluate it on the domain grid
  Eval_grid <- eval_on_domain(tilde, domain, n = npts, args$dots)
  inf_locations <- is.infinite(Eval_grid[,2])
  if (any(inf_locations))
    Eval_grid[inf_locations, 2] <- NaN

  # place the breaks at the singularities
  if (length(singularities) > 0) {
    matches <- Eval_grid[,1] %in% singularities
    Eval_grid[matches,2] <- NaN
    addins <- which(! singularities %in% Eval_grid[,1])
    if (length(addins) > 0) {
      Singularities <- data.frame(x=singularities[addins], y=NaN)
      names(Singularities) <- names(Eval_grid)
      Eval_grid <- bind_rows(Singularities)
    }
  }

  # Look for any Inf's. Replace them with NaN so ggplot2 doesn't connect the lines
  # the_infs <- Eval_grid[,2] == Inf
  # if (any(the_infs)) Eval_grid[the_infs, 2] <- NaN

  if (is.null(object)) object <- 
    ggplot(Eval_grid, aes(x = !!as.name(names(Eval_grid)[1]),
                          y = .output.))

  P <- object + geom_line(data = Eval_grid, 
                          aes(x = !!as.name(names(Eval_grid)[1]),
                              y = .output.),
                          color=color, alpha=alpha)

  # put the label in  place
  if (label_text != "") {
    n <- nrow(Eval_grid)
    row_num <- pmax(1, pmin(n, round(n * label_x)))
    xpos <- Eval_grid[row_num,1]
    ypos <- Eval_grid$.output.[row_num]
    P <- P + geom_text(x = xpos, y = ypos, label=label_text,
                       vjust=label_vjust, color=label_color, alpha=label_alpha)
  }

  P
}
