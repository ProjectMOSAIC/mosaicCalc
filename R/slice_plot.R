#' Plot a function of a single variable
#'
#'
#' @param label_text character string label to place near the graph curve. Default: none.
#' @param label_x number between 0 and 1 indicating the horizontal placement  of the `label_text`.
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

  Eval_grid <- mosaicUSAFA:::eval_on_domain(formula, domain, n = npts)
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
