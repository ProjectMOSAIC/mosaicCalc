#' Bind specific values to arguments of a function
#'
#' Does partial evaluation, creating a new function that is has only the
#' unbound arguments as inputs
#' @param fun the function to partially evaluate.
#' @param \dots bindings for parameters to `fun`
#'
#'
#' @examples
#' f <- makeFun(x + 10*y + 100*z ~ x) # y and z are unbound parameters
#' bindArgs(f, y=3)
#' bindArgs(f, z = y)
#' bindArgs(f, y=3, z=4)
#' bindArgs(f, x=2, y=3, z=4)
#' @export
bindArgs <- function(fun, ...) {
  # no point in changing primitive functions (like sin())
  if (is.primitive(fun)) return(fun)
  # But reconsider eventually. Maybe should return a new function calling
  # sin() on it's bound parameter.

  bindings <- enexprs(...)
  bind_names <- names(bindings)
  if (any(nchar(bind_names) == 0)) {
    stop("Unnamed arguments not allowed. Each argument should be in a form like x=19")
  }
  fun_args <- formals(fun)

  if (!any(bind_names %in% names(fun_args))) {
    outsiders <- setdiff(bind_names, names(fun_args))
    outsiders <- paste(outsiders, collapse=", ")
    stop(paste("Arguments to bindArgs", outsiders, "must be arguments to the function being bound."))
  }

  substitutions <- paste(bind_names, "<-", bindings, collapse="; ")
  subs <- parse(text=substitutions)
  nlines <- length(subs)

  bod <- body(fun)
  if (is.call(bod)) {
    tmp <- quote({x})
    tmp[[2]] <- bod
    bod <- tmp
  }

  if (inherits(bod, "{")) {
    inds <- 2:length(bod)
    bod[inds+nlines] <- bod[inds]
    bod[1 + 1:nlines] <- subs
    body(fun) <- bod
  } else {
    stop("Should not get here. What kind of thing is bod?")
  }

  formals(fun)[bind_names] <- NULL

  return(fun)
}
