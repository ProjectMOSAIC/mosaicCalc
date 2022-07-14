#' Turn a 1-line function into an inline formula
#'
#' @param fun a function, such as produced by `makeFun()`.
#' @param ex An expression, such as `quote(x^2)`.

#' @rdname inline
simplify_fun <- function(fun) {
  bod <- paste(deparse(body(fun)), collapse=" ")
  if (class(bod) != "{") { # it's a one-line function.
    sbod <- try(Ryacas::as_r(Ryacas::yac(glue::glue("Simplify({bod})"))))
    if (inherits(sbod, "try-error")) return(fun)
    body(fun) <- sbod
  }
  return(fun)
}

#' @param ex an expression
#' @param env the environment for the expression
#' @param old the name to be replaced
#' @param new the name to be substituted in for old
#' @rdname inline
inline_expr <- function(ex, old, new, env) {
  if (length(ex) != 2) return(list(ex=ex, fargs=NULL))

  # Special cases
  if (ex[[1]] == "sqrt") {
    # Need to convert to exponentiation with 1/2
    template <- quote(x^0.5)
    template[[2]] <- ex[[2]] # replace the "x" with the relevant contents.
    return(list(ex=template, fargs=NULL))
  }
  
  # handle the head if it's already defined
  # otherwise leave it alone
  fun <- try(eval(ex[[1]], envir=env), silent=TRUE)
  if (!is.function(fun)) return(list(ex=ex, fargs=NULL))
  fbody <- body(fun)
  if (is.null(fbody) || inherits(fbody, "{")) return(list(ex=ex, fargs=NULL)) # a primitive or multi-line function
  if (fbody[[1]] == ".Call") return(list(ex=ex, fargs=NULL)) # cannot handle special functions
  fargs <- formals(fun)
  old_name <- as.name(names(formals(fun))[1])
  one <- replace_arg_in_expr(fbody, old_name, ex[[2]])
  
  list(ex=one, args=fargs)
}

#' @param old the name to be replaced as produced by as.name()
#' @param new the name to be substituted in for arg_name

#' @rdname inline
replace_arg_in_expr <- function(ex, old, new) {
  if (length(ex) == 1) {
    if (ex == old) return(new)
    else return(ex)
  } else {
    for (k in 1:length(ex)) {
      ex[[k]] <- replace_arg_in_expr(ex[[k]], old, new)
    }
  }
  return(ex)
}


