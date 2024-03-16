#' Create a numerical anti-derivative function which can be
#' called with one or many values of the w.r.t. input
#' 
#' This will typically be called directly from `antiD()` when an
#' integral can't be handled symbolically.
#' 
#' @param tilde Tilde expression for the function to be anti-differentiated. 
#' Right-hand side will be the w.r.t. variable
#' @param \dots arguments and parameters to the function described by `tilde`
#' @param lower Optional lower bound of integration. Useful to avoid domain
#' problems with the function being integrated, but not generally needed.
#' @param .tol Numerical tolerance for the integration
#' 
#' @returns a function with the w.r.t. variable as the first argument. The function
#' is a wrapper around numerical integration routines.
#' 
create_num_antiD <- function(tilde, ..., lower = NULL, .tol=0.0001) {
  wrt <- all.vars(tilde[[3]])[1] # right hand side of formula
  little_f <- makeFun(tilde) %>% bind_params(...)
  fargs <- formals(little_f)
  
  the_function <- function(){ 
    dots <- formals() # grab the formals of this function
    
    # some formals may be unbound.
    # Bind them to values from arguments to this function
    # (Those arguments will be added later, near the end of create_num_antiD().)
    unbound <- character(0)
    for (nm in names(dots)) {
      val <- try(get(nm), silent=TRUE)
      if (inherits(val, "try-error")) unbound <- c(unbound, nm)
      else {
        #if (nm != wrt && length(val) > 1) stop("All parameters must have length 1.")
        dots[[nm]] <- val
      }
    }
    
    if (length(unbound) > 0) {
      stop(paste(
        "Need to bind all parameters when evaluating numerical anti-derivative. Parameters",
        paste0("<", unbound, ">", collapse=" & "),
        "have not been given a numerical value."))
    }
    
    x <- get(wrt)
    dots[[wrt]] <- NULL # shorten the list of arguments
    
    # at this point, we have all needed parameters in <dots> and w.r.t. values in <x>
    res <- numeric(length(x))
    if (!is.null(lower)) {
      if (x[[1]] == -Inf) { # Deal with bug in stats:integrate() when 
        # working with an upper-bound of -Inf.
        res[1] <- -do.call(
          stats::integrate,
          c(list(little_f, x[1], lower), dots, list(rel.tol=.tol)))$value
      } else {
        res[1] <- do.call(
          stats::integrate,
          c(list(little_f, lower, x[1]), dots, list(rel.tol=.tol)))$value
      }
    } else res[1] <- 0
  
    if (length(x) > 1) {
      for (k in 2:length(x)) {
        res[k] <- do.call(
          stats::integrate,
          c(list(little_f, x[k-1], x[k]), dots, list(rel.tol=.tol)))$value
      }
    }
    
    cumsum(res)
    
   
  } 
  formals(the_function) <- fargs
  
  the_function
}

# IN THE FUNCTION THAT CALLS THIS, add the constant of integration to the result

