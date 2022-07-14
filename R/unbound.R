#' Identifying unbound inputs to a function
#'
#' `unbound()` finds if there are any unbound parameters in a function. This can be 
#' useful for checking before handing a function over to a numerical routine.
#' `bind_params()` lets you add parameter bindings or override existing ones.
#'
#' @param f a function (not a tilde expr.)
#' @param \dots bindings for parameters
#' 
unbound <- function(f) {
  ff <- formals(f)
  names(ff)[lapply(formals(f), as.character) %>% unlist() == ""]
}

#' @rdname unbound
#' @export
bind_params <- function(f, ...) {
  new_values <- list(...) # there can be extras
  # if the ... contain only one item, a list, undue the list() make in the previous line
  if (length(new_values) == 1 && is.list(new_values[[1]])) new_values <- new_values[[1]]
  params <- formals(f)
  for (k in names(params)) {
    if (k %in% names(new_values) && !inherits(new_values[[k]], "name")) params[k] <- new_values[[k]]
  }
  formals(f) <- params
  
  f
}

