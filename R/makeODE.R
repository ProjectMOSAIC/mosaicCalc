#' Create a dynamics object for use in `integrateODE()` and the ODE graphics
#' 
#' An ODE object consists of some dynamics, initial conditions, parameter values, 
#' time domain, etc. 
#' 
#' @param \dots The components of an ODE and/or a set of other ODE objects. 
#' 
#' By default, the time step is set to `dt = 0.01`. Change it with `dt = ` value.
#' 
#' @examples 
#' SIR <- makeODE(dS~ -a*S*I, dI ~ a*S*I - b*I, a=0.0026, b=.5, S=762, I=1)
#' soln <- integrateODE(SIR, domain(t=0:20))
#' 
#' @returns a list containing various functions and values specifying the
#' ODE.
#' 
#' @export
makeODE <- function(...) {
  # process the arguments
  args <- list(...)
  
  if ("dt" %in% names(args)) {
    time_step <- args[["dt"]]
    args[["dt"]] <- NULL
  } else if ("dt" %in% names(args[[1]])) {
    time_step <- args[[1]][["dt"]]
  } else {
    time_step <- 0.01
  }
  
  # No longer accepting the old <tdur> argument
  if ("tdur" %in% names(args)) {
    stop("tdur =  is an obsolete argument. Use domain() to set start and finish, and dt = to set time step.")
  }
  
  
  Dyn_object <- list(names = character(0), functions = NULL, 
                     values=NULL, dt=time_step)
  dynInds <- which(sapply(args, function(x) inherits(x, "dynamics")))
  if (length(dynInds) > 0) {
    for (ind in dynInds) {
      Dyn_object$names <- c(Dyn_object$names, args[[ind]]$names)
      Dyn_object$functions <- c(Dyn_object$functions, args[[ind]]$functions)
      Dyn_object$values <- join_values(Dyn_object$values, args[[ind]]$values)
    }
  } 
  args[dynInds] <- NULL # strip them out
  
  # form the dynamics from the tilde expressions
  if (length(args) > 0) {
    formInds <- which( sapply(args, function(x) inherits(x, 'formula') ) )
    dnames <- c()
    dfuns <- c()
    for (k in seq_along(formInds) ) {
      form = args[[formInds[k]]]
      nm = form[[2]] # should be name type so double the brackets [[ ]]
      if ( ! inherits(nm, "name") ) stop(paste("Invalid name on LHS of formula",nm))
      nm = as.character(nm)
      if (grepl("^d",nm)) {
        dnames[k] <- sub("^d","",nm) # character string with the name
      } else {
        dnames[k] <- nm
        message("The name on the left side of a diff. eq. tilde expression should be
                prefixed with 'd', as in dx ~ x+y")
      } 
      dfuns[k] <- parse(text=form[3]) # an expression so single [ ]
    }
    Dyn_object$names <- c(Dyn_object$names, dnames)
    Dyn_object$functions <- c(Dyn_object$functions, dfuns)
    # Add these in to the return structure
    args[formInds] <- NULL
  }
  
  # Get the domain, if any
  if (length(args) > 0) {
    durInds <- which( sapply(args, function(x) inherits(x, 'xdomain') ) )
    if (length(durInds) > 0) {
      Dyn_object$domain <- args[[durInds[length(durInds)]]] # last one
      args[durInds] <- NULL
    }
  }
  
  # Get the domain for the graphics space (e.g. for streamlines())
  if (length(args) > 0) {
    spaceInds <- which( sapply(args, function(x) inherits(x, 'xydomain') ) )
    if (length(spaceInds) > 0) {
      Dyn_object$domain <- args[[spaceInds[length(spaceInds)]]] # last one
      args[spaceInds] <- NULL
    }
  }
  
  
  # Get initial conditions and parameter values
  with_names <- list()
  if (length(args) > 0) {
    with_names <- args[nchar(names(args)) > 0]
    extras <- setdiff(names(with_names), all.vars(Dyn_object$functions)) # are there superfluous names
    if (length(extras) > 0)
      warning(paste("Parameters",
                    paste0("<", extras, ">", collapse=" & "),
                    "are not used in the differential equations."))
  }
  
  # Polish up the object
  
  Dyn_object$values <- join_values(Dyn_object$values, with_names)
  Dyn_object$params <- Dyn_object$values[setdiff(names(Dyn_object$values), Dyn_object$names)]
  Dyn_object$vfun <- dyn_vector_fun(Dyn_object$functions, Dyn_object$names,
                                    Dyn_object$params)
    
  class(Dyn_object) <- c("list", "dynamics")
  
  return(Dyn_object)
}

join_values <- function(old, new) {
  old[names(new)] <- new
  
  old
}

remove_duplicates <- function(dyn_object) {
  duplicates <- which(duplicated(dyn_object$names))
  if (length(duplicates) > 0) {
    dyn_object$names <- dyn_object$names[-duplicates]
    dyn_object$functions <- dyn_object$functions[-duplicates]
  }
  
  dyn_object
}

dyn_vector_fun <- function(exprs, names, params) {
  v <- 1 # avoid problem in R CMD check
  res <- function() {
    state_vec <- as.list(v)
    names(state_vec) <- names
    unlist(lapply(exprs, function(x) eval(x, envir=state_vec)))
  } 
  formals(res) <- c(alist(v=), params)
  
  res
}
