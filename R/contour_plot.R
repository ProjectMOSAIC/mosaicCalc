#' Contour plots of functions of two variables
#'
#' Creates a ggplot2-compatible contour plot of a function
#' of two variables.
#'
#' @param \dots Canonical first "three" arguments: `[Previous layer]`, tilde expression, [domain]
#' @param npts Integer number of points on each axis at which to evaluate the function
#' @param labels Logical flag: label the contours
#' @param filled Logical flag: fill between the contours
#' @param contours_at Vector of numbers. Contours will be drawn
#' at these levels of the output.
#' @param n_contours hint at number of contours to show
#' @param n_fill Integer number of points for calculating fill
#' @param alpha Transparency of contours in `[0-1]`
#' @param fill_alpha Transparency of fill
#' @param label_alpha Likewise, for contour labels
#' @param label_placement A number, between 0 and 1, suggesting where to place
#' the contour labels. Default: 0.5. This can be useful when there are two contour layers in one plaot.
#' @param contour_color Set to a string, e.g. `"blue"` for contours
#' to be drawn in a fixed color
#' @param label_color Defaults to `contour_color`. Can be set to a
#' @param skip Small integer. Skip this many contours between labeled contours
#' @param guide Logical flag, whether to show a color guide.
#' @param guide_title Character string  for title of guide (if any)
#' @param color_scale See ggplot2 color scales.
#' @param fill_scale See ggplot2 fill scales
#'
#' @examples
#' \dontrun{
#' contour_plot(sin(0.2*x*y) ~ x & y, domain(x=-10.3:4.5, y=2:7.5))
#' contour_plot(x + y ~ x & y)
#' contour_plot(sin(0.2*x*y)) # but better to use tilde expression
#' }
#' 
#' @returns ggplot2 graphics layers
#'
#' @importFrom  ggplot2 aes  ggplot geom_line geom_text scale_color_viridis_c scale_fill_viridis_c guides
#' @importFrom  metR geom_contour_fill geom_contour2 geom_text_contour
#' @export
contour_plot <- function(..., # canonical first three arguments
                         npts = 100,
                         labels = TRUE,
                         filled = TRUE,
                         contours_at = NULL,
                         n_contours = 10,
                         n_fill = 50,
                         alpha = 1,
                         fill_alpha = 0.1,
                         label_alpha  = 1,
                         label_placement = 0.5,
                         contour_color = "blue",
                         label_color = contour_color,
                         skip = 1,
                         guide  = FALSE,
                         guide_title = "output",
                         color_scale = scale_color_viridis_c(),
                         fill_scale = scale_fill_viridis_d()) {
  args <- first_three_args(...)
  object <- args$gg
  tilde <- args$tilde
  # Accepts a one-side tilde expression, turning it into a two-sided one.
  if (length(tilde)==2) tilde <- infer_RHS(tilde[[2]])
  domain <- args$domain
  
  if (!is.null(object)) {
    # get domain from ggplot object, unless domain is already specified
    if (is.null(domain)) {
      look_for <- all.vars(tilde[[3]]) # the input variable name
      if (all(look_for %in% names(object$data))) {
        domain <- list()
        domain[[look_for[1]]] <- range(object$data[[look_for[1]]])
        domain[[look_for[2]]] <- range(object$data[[look_for[2]]])
      } else {
        stop("Must specify bounds or use same two input names as previous layer.")
      }
    }
  } 
  
  # make sure domain is specified
  if (is.null(domain)) {
    msg <- glue::glue("Must specify domain after tilde expr, e.g. \n          slice_plot({capture.output(tilde)}, domain({all.vars(tilde[[3]])[1]}=0:10), domain({all.vars(tilde[[3]])[2]}=-5:5))")
    stop(msg)
  }

  # Check that tilde expression is a function of two variables
  independent_vars <- all.vars(tilde[[3]])
  if (length(independent_vars) != 2)
    stop("Contour plot tilde must have two names on RHS of tilde.")

  # Check for unbound parameters.
  testf <- makeFun(tilde) %>% bind_params(args$dots)
  unbound_args <- unbound(testf) %>% setdiff(independent_vars)
  if (length(unbound_args) > 0) {
    msg <- paste0("Parameter", ifelse(length(unbound_args) > 1, 's ', ' '), 
                  paste0("<", unbound_args, ">", collapse=", "), 
                  " without specified numerical values.")
    stop(msg)
  }
  
  # Check that the function itself depends on both variables
  vars_in_function <- all.vars(tilde[[2]])
  null_y_flag <- FALSE
  if (! all(independent_vars %in% vars_in_function)) {
    null_y_flag <- TRUE
    warning("No dependence of function on y variable. Contour labels may be misplaced.")
  }
  Eval_grid <- eval_on_domain(tilde, domain, n = npts, args$dots)
  # If it's a boolean, turn it into a number
  Eval_grid$.output. <- as.numeric(Eval_grid$.output.)
  # Check that specified contours are in range
  if (!is.null(contours_at)) {
    if (all(contours_at > max(Eval_grid$.output., na.rm=TRUE)) &&
        all(contours_at < min(Eval_grid$.output., na.rm=TRUE))) {
      warning("Specified contours outside level of function. Showing median.")
      contours_at <- median(Eval_grid$.output., na.rm=TRUE)
    }
  }

  coarse_breaks <-
    pretty(range(Eval_grid$.output., na.rm=TRUE), n = n_contours)
  fine_breaks <- seq(min(coarse_breaks),
                     max(coarse_breaks),
                     length = n_fill)
  if (!is.null(contours_at)) {
    coarse_breaks <- contours_at # override the default
  }

  input_names <-  names(Eval_grid)
  input_names <- input_names[input_names != ".output."]
  the_mapping <- aes(x = !!as.name(input_names[1]),
                     y = !!as.name(input_names[2]))
  if (is.null(object)) {
    object <- ggplot(Eval_grid, the_mapping)
  } else { # get rid of previous aesthetic
    object$mapping <- the_mapping 
  }

  P <- object

  if (filled) {
    P <- P + metR::geom_contour_fill(aes(z = .output., fill = filled),
                                     data = Eval_grid,
                                     alpha = fill_alpha,
                                     breaks = fine_breaks)
  }
  
  P <- P + metR::geom_contour2(aes(z = .output.),
                               colour = contour_color,
                               data = Eval_grid,
                               alpha = alpha,
                               breaks = coarse_breaks)
  
  if (labels) {
    if (skip > length(coarse_breaks) -1 )
      skip <- length(coarse_breaks) - 1

    # When there is no dependence at all on y, the contour
    # drawing routine creates something that can't be drawn.
    if (null_y_flag) {
      # add a tiny bit of dependence on y
      Eval_grid$.output. <- Eval_grid$.output. + 0.0000001 * Eval_grid[[2]]
    }
    P <- P + metR::geom_label_contour(aes(z = .output.),
                                     data = Eval_grid,
                                     color=label_color,
                                     label.placer = metR::label_placer_fraction(frac=label_placement),
                                     label.padding = grid::unit(0.25, "lines"),
                                     breaks = coarse_breaks, skip=skip,
                                     alpha=label_alpha)
  }

  g_contours <- g_fill <- ggplot2::guide_legend(guide_title)
  if (filled) g_contours <- "none"
  if (!guide) g_fill <- g_contours <- "none"

  P +
    color_scale  +
    fill_scale  +
    guides(fill=g_fill, color=g_contours)
}
