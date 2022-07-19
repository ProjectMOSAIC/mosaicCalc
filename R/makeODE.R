#' Create a dynamics object for use in `integrateODE()` and the ODE graphics
#' 
#' An ODE object consists of some dynamics, initial conditions, parameter values, 
#' time domain, etc. 
#' 
#' @param \dots The components of an ODE and/or a set of other ODE objects
#' 
#' @examples 
#' SIR <- makeODE(dS~ -a*S*I, dI ~ a*S*I - b*I, a=0.0026, b=.5, S=762, I=1)
#' soln <- integrateODE(SIR, domain(t=0:20))
#' 
#' @export
makeODE <- function(...) {
  # process the arguments
  args <- list(...)
  
  if ("dt" %in% names(args)) {
    time_step <- args[["dt"]]
    args["dt"] <- NULL
  } else {
    time_step <- 0.01
  }
  
  # for backward compatibility, pull out any argument named <tdur>.
  if ("tdur" %in% names(args)) {
    tdur <- args[["tdur"]]
    args["tdur"] <- NULL
    if (length(tdur) > 1) 
      args[length(args)+1] <- domain(t=c(tdur[1], tdur[2]))
    if (length(tdur) == 3) time_step <- tdur[3]                                
  }
  
  
  Dyn_object <- list(names = character(0), functions = NULL, 
                     values=NULL, domain=NULL, dt=0.01, Pprev=NULL)
  if (inherits(args[[1]], "gg")) {
    Dyn_object$Pprev <- args[[1]]
    args[1] <- NULL
  }
  dynInds <- which(sapply(args, function(x) inherits(x, "dynamics")))
  if (length(dynInds) > 0) {
    for (ind in dynInds) {
      Dyn_object$names <- c(Dyn_object$names, args[[ind]]$names)
      Dyn_object$functions <- c(Dyn_object$functions, args[[ind]]$functions)
      Dyn_object$values <- join_values(Dyn_object$values, args[[ind]]$values)
      if (length(args[[ind]]$domain) > 0) # use the last one specified
        Dyn_object$domain <- args[[ind]]$domain
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
      if (grep("^d",nm)!=1) {
        dnames[k] <- nm
        warning("Dynamical variables should start with 'd'")
      } else {
        dnames[k] <- sub("^d","",nm) # character string with the name
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
      Dyn_object$xydomain <- args[[spaceInds[length(spaceInds)]]] # last one
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
