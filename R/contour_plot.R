#' Contour plots of functions of two variables
#'
#'
#' @importFrom  ggplot2 aes  ggplot scale_color_viridis_c scale_fill_viridis_c guides
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

  Eval_grid <- mosaicUSAFA:::eval_on_domain(formula, domain, n = npts)

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
    P <- P + metR::geom_text_contour(aes(z = .output.),
                                     data = Eval_grid,
                                     color=label_color,
                                     breaks = coarse_breaks, skip=skip,
                                     alpha=label_alpha)
  }

  g_contours <- g_fill <- guide_legend(guide_title)
  if (filled) g_contours <- "none"
  if (!guide) g_fill <- g_contours <- "none"

  P +
    color_scale  +
    fill_scale  +
    guides(fill=g_fill, color=g_contours)
}
