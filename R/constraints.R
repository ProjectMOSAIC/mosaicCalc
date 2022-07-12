#' Graphics for constraints
#'
#' These functions are intended to annotate contour plots with constraint regions. Inequality
#' constraints are shaded where the constraint is NOT satisfied. Equality constraints
#' are shaded in a small region near where the constraint is satisfied.
#'
#' @param previous can be ignored by user. It supports the pipe syntax for layering
#' graphics.
#' @param tilde a tilde expression specifying the constraint. For an inequality constraint
#' this should be a logical expression that is TRUE where the constraint is satisfied.
#' For an equality constraint, the left-hand side of the tilde expression should
#' be zero where the constraint IS satisfied.
#' @param domain as in contour_plot(), the domain over which to graph the constraint
#' @param npts a number specifying how finely to divide the domain in each direction. Default
#' is 100, but this gives a discernably pixelated appearance to the shading.  200 or 300
#' is more appropriate for publication-quality graphics.
#' @param fill the color to use for shading
#' @param alpha the opacity of the shading
#'
#' @examples
#' inequality_constraint(x + y > 2 ~ y + x, domain(y=0:3, x=0:2))
#' equality_constraint(x + y - 2 ~ y + x, domain(y=0:3, x=0:2), npts=200, alpha=.3, fill="red")
#'

#' @export
inequality_constraint <- function(previous=NULL, tilde, domain, npts=100, fill="blue",alpha=1) {
  if (inherits(previous, "formula")) {
    domain <- tilde
    tilde <- previous
    previous <- NULL
  }
  Eval_grid <- eval_on_domain(tilde, domain, n = npts)
  Eval_grid$.output. <- 1 - Eval_grid$.output.
  nms <- names(Eval_grid)
  graph_formula <- as.formula(paste(nms[2], "~", nms[1]))
  previous %>% gf_raster(graph_formula, alpha= ~ alpha*.output., fill=fill, data = Eval_grid) %>%
    gf_refine(scale_alpha_identity())
}
#' @rdname inequality_constraint
#'
#' @export
equality_constraint <- function(previous=NULL, tilde, domain, npts=100, fill="blue",alpha=1) {
  if (inherits(previous, "formula")) {
    domain <- tilde
    tilde <- previous
    previous <- NULL
  }
  Eval_grid <- eval_on_domain(tilde, domain, n = npts)
  threshold <- quantile(Eval_grid$.output.^2, 0.02)
  Eval_grid$.output. <- as.numeric(Eval_grid$.output^2 <= threshold)
  nms <- names(Eval_grid)
  graph_formula <- as.formula(paste(nms[2], "~", nms[1]))
  previous %>%gf_raster(graph_formula, alpha= ~ alpha*.output., fill=fill, data = Eval_grid) %>%
    gf_refine(scale_alpha_identity())
}


