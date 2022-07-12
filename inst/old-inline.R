#' Turn a 1-line function into an inline formula
#'
#' @param LHS an expression or formula
#'
#' @examples
#' infer_RHS(x^2 ~ x) # Formula is unchanged
make_tilde_inline <- function(tilde) {
  vars <- all.vars(rlang::f_rhs(tilde))
  rlang::f_lhs(tilde) <-
    inline_expr(rlang::f_lhs(tilde),
                as.name(vars[1]), as.name(vars[1]),
                environment(tilde))

  tilde
}

#' apply simplify_mult() to a function to create a new function.
#' @rdname infer_RHS
#' @export
simplify_fun <- function(fun) {
  bod <- paste(deparse(body(fun)), collapse=" ")
  if (length(body(fun)) == 1) {
    sbod <- try(Ryacas::as_r(Ryacas::yac(glue::glue("Simplify({bod})"))))
    if (inherits(sbod, "try-error")) return(fun)
    # bod <- body(fun)
    # sbod <- simplify_mult(bod)
    body(fun) <- sbod
  }
  return(fun)
}

#' Try to reduce multiplication in a formula
#' @rdname infer_RHS
simplify_mult <- function(ex) {
  if (length(ex) == 1) return(ex)
  if (length(ex) == 2) {
    replacement <- simplify_mult(ex[[2]])
    if (ex[[1]] == "(") return(replacement)

    ex[[2]] <- replacement
    value <- try(eval(ex), silent=TRUE)
    if (inherits(value, "try-error")) return(ex)
    else return(value)
  }
  # must be length 3

  # 0. Anything we can handle directly numerically
  res <- try(eval(ex, envir=NULL, enclos=NULL), silent=TRUE) #DTK changed 03/02/2022
  if (!inherits(res, "try-error")) return(res)


  # handle the simple cases
  ex[[2]] <- simplify_mult(ex[[2]])
  ex[[3]] <- simplify_mult(ex[[3]])
  # 1. Multiplication by 1 or 0
  if (ex[[1]] == "*") {
    if (ex[[2]] == 1) return(ex[[3]])
    if (ex[[3]] == 1) return(ex[[2]])
    if (ex[[2]] == 0 || ex[[3]] == 0) return(0)
    if (identical(ex[[2]], ex[[3]])) { # multiplication with itself
      ex[[1]] <- as.name("^")
      ex[[3]] <- 2
      return(ex)
    }
  }
  # 2. Addition with 0
  if (ex[[1]] == "+") {
    if (ex[[2]] == 0) return(ex[[3]])
    if (ex[[3]] == 0) return(ex[[2]])
    if (identical(ex[[2]], ex[[3]])) { # addition with itself
      ex[[1]] <- as.name("*")
      ex[[2]] <- 2
      return(ex)
    }
  }

  # if (ex[[1]] == "*") {
  #   two <- try(eval(ex[[2]]), silent=TRUE)
  #   if (is.numeric(two)) {
  #     three <- simplify_mult(ex[[3]])
  #       if (ex[[3]][[1]] == "*" && is.numeric(ex[[3]][[2]])) {
  #         ex[[3]][[2]] = eval(ex[[3]][[2]])*val
  #         ex <- ex[[3]]
  #       }
  #     }
  #   }
  # }


  if (ex[[1]] == "+" || ex[[1]] =="*") {
    # if there is a number in ex[[2]] or ex[[3]], make sure it's in ex[[2]]
    two <- ex[[2]]
    if (!is.numeric(two)) {
      ex[[2]] <- ex[[3]]
      ex[[3]] <- two
    }
    two <- ex[[2]]
    if (!is.numeric(two)) {
      # make sure a simple symbol is first if there is one
      if (!is.name(two)) {
        ex[[2]] <- ex[[3]]
        ex[[3]] <- two
      }
    }
  }
  if (is.numeric(ex[[2]]) && length(ex[[3]])== 3 && ex[[1]] == "*" && ex[[3]][[1]] == "*") {
    three <- simplify_mult(ex[[3]][[2]])
    if (is.numeric(three)) {
      ex[[3]][[2]] <- ex[[2]]*three
      return(ex[[3]])
    }
  }
  if (is.name(ex[[2]]) && length(ex[[3]])== 3 && ex[[3]][[1]] == "*") {
    three <- simplify_mult(ex[[3]][[2]])
    if (is.name(three) && identical(three, ex[[2]])) {
      power_form <- quote(x^2)
      power_form[[2]] <- three
      ex[[3]][[2]] <- power_form
      return(ex[[3]])
    }
  }

  ex
}

#' @param ex an expression
#' @param env the environment for the expression
#' @param old the name to be replaced
#' @param new the name to be substituted in for old
#' @rdname infer_RHS
#' @export
inline_expr <- function(ex, old, new, env) {
  if (is.null(env)) return(ex) # unchanged
  if (length(ex) == 1) {
    if (ex == old) return(new)
    return(ex)
  } else {
    for(k in 2:length(ex))
      ex[[k]] = inline_expr(ex[[k]], old, new, env)

    # handle the head if it's already defined
    # otherwise leave it along
    f <- try(eval(ex[[1]], envir=env), silent=TRUE)
    if (inherits(f, "try-error")) return(ex)
    fargs <- formals(f)

    if (is.null(environment(f)) || is.null(fargs) || length(fargs) > 1) {
      # ineligible for inlining
    } else {
      fbody <- body(f)

      if (class(fbody) == "call") {
      farg <- as.name(names(formals(f))[1])
      one <- replace_arg_in_expr(fbody, farg, ex[[2]])
      two <- inline_expr(one, old, new, env)
      ex <- two
      }
    }

  }
  ex
}



#' @param old the name to be replaced as produced by as.name()
#' @param new the name to be substituted in for arg_name
#' @rdname infer_RHS
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


