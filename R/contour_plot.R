#' Contour plots of functions of two variables
#'
#' Creats a ggplot2-compatible contour plot of a function
#' of two variables.
#'
#'
#' @param object (Can be ignored. Used to pass previous layers into  this one.)
#' @param formula A tilde formula with two independent variables, e.g. `x*y~x+y`
#' @param domain A list giving the ranges for the two independent variables. See examples.
#' @param npts Integer number of points on each axis at which to evaluate the function
#' @param labels Logical flag: label the contours
#' @param filled Logical flag: fill between the contours
#' @param contours_at Vector of numbers. Contours will be drawn
#' at these levels of the output.
#' @param nfill Integer number of points for calculating fill
#' @param alpha Transparency of contours in [0-1]
#' @param fill_alpha Transparency of fill
#' @param label_alpha Likewise, for contour labels
#' @param contour_color Set to a string, e.g. `"blue"` for contours
#' to be drawn in a fixed color
#' @param label_color Defaults to `contour_color`. Can be set to a
#' @param skip Small integer. Skip this many contours between labeled contours
#' @param guide Logical flag, whether to show a color guide.
#' @param guide_title Character string  for title of guide (if any)
#' @param color_scale See ggplot2 color scales.
#' @param fill_scale See ggplot2 fill scales
#'
#' to a fixed color.
#'
#' @importFrom  ggplot2 aes  ggplot geom_line geom_text scale_color_viridis_c scale_fill_viridis_c guides
#' @importFrom  metR geom_contour_fill geom_contour2 geom_text_contour
#' @export
contour_plot <- function(object,  formula,  domain,
                         npts = 100,
                         labels = TRUE,
                         filled = TRUE,
                         contours_at = NULL,
                         n_contours = 10,
                         n_fill = 50,
                         alpha = 1,
                         fill_alpha = 0.1,
                         label_alpha  = 1,
                         contour_color = NULL,
                         label_color =
                           ifelse(!is.null(contour_color), contour_color, "black"),
                         skip = 1,
                         guide  = FALSE,
                         guide_title = "output",
                         color_scale = scale_color_viridis_c(),
                         fill_scale = scale_fill_viridis_c(),
                         ...) {
  if (rlang::is_formula(object)) {
    # shift first two arguments down to  become formula
    # and domain
    if (!missing(formula)) domain <- formula
    formula <- object
    object <- NULL
    if (missing(domain) || is.null(domain))
      stop("Domain must be specified when there is no preceeding layer.")
  } else if (inherits(object, "ggplot")) {
    if (missing(domain)) {
      look_for <- all.vars(formula[[3]]) # the input variable names
      if (all(look_for %in% names(object$data))) {
        domain <- list()
        domain[[look_for[1]]] <- range(object$data[[look_for[1]]])
        domain[[look_for[2]]] <- range(object$data[[look_for[2]]])
      } else {
        stop("Must specify domain or use same x/y variables as previous layer.")
      }
    }
  } else {
    stop("First argument (or pipe input) must be either a formula or a ggplot  layer.")
  }

  # Check that formula  is a function of two variables
  independent_vars <- all.vars(formula[[3]])
  if (length(independent_vars) != 2)
    stop("Contour plot formula must have two one vars on RHS of tilde.")

  # Check that the function itself depends on both variables
  vars_in_function <- all.vars(formula[[2]])
  null_y_flag <- FALSE
  if (! all(independent_vars %in% vars_in_function)) {
    null_y_flag <- TRUE
    warning("No dependence of function on y variable. Contour labels may be misplaced.")
  }
  Eval_grid <- mosaicCalc:::eval_on_domain(formula, domain, n = npts)

  # Check that specified contours are in range
  if (!is.null(contours_at)) {
    if (all(contours_at > max(Eval_grid$.output., na.rm=TRUE)) &&
        all(contours_at < min(Eval_grid$.output., na.rm=TRUE))) {
      warning("Specified contours outside level of function. Showing median.")
      contours_at <- median(Eval_grid$.output., na.rm=TRUE)
    }
  }

  coarse_breaks <-
    pretty(range(Eval_grid$.output.), n = n_contours)
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
  if (is.null(object))
    object <- ggplot(Eval_grid, the_mapping)

  P <- object

  if (filled) {
    P <- P + metR::geom_contour_fill(aes(z = .output., fill = ..level..),
                            data = Eval_grid,
                            alpha = fill_alpha,
                            breaks = fine_breaks)
  }

  if (is.null(contour_color)) {
    P <- P + metR::geom_contour2(aes(z = .output., colour = ..level..),
                                 data = Eval_grid,
                                 alpha = alpha,
                                 breaks = coarse_breaks, ...)
  } else {
    P <- P + metR::geom_contour2(aes(z = .output.),
                                 colour = contour_color,
                                 data = Eval_grid,
                                 alpha = alpha,
                                 breaks = coarse_breaks, ...)
  }

  if (labels) {
    if (skip > length(coarse_breaks) -1 )
      skip <- length(coarse_breaks) - 1

    # When there is no dependence at all on y, the contour
    # drawing routine creates something that can't be drawn.
    if (null_y_flag) {
      # add a tiny bit of dependence on y
      Eval_grid$.output. <- Eval_grid$.output. + 0.0000001 * Eval_grid[[2]]
    }
    P <- P + metR::geom_text_contour(aes(z = .output.),
                                     data = Eval_grid,
                                     color=label_color,
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
