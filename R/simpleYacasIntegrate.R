#' Interface to integration using Ryacas
#'
#' @param tilde The tilde expression for the integration
#' @param \dots Bindings for parameters in `tilde`
simpleYacasIntegrate <- function(tilde, ...) {
  the_expression <- deparse(rlang::f_lhs(tilde))
  fun <- makeFun(tilde, ..., 
                 suppress.warnings=TRUE,
                 strict.declaration = FALSE, 
                 use.environment = TRUE)
  vars <- all.vars(rlang::f_rhs(tilde), unique=FALSE)
  the_expression <- paste(fix_names_for_yacas(the_expression), collapse="")

  R_result <- try(
    Ryacas::yac(
      glue::glue(
        "Simplify(Integrate({vars[[1]]}) {the_expression});"
      )
    ),
    silent=TRUE
  )
  
  if (inherits(R_result, "try-error") || grepl("Integrate\\(", R_result)) {
    return(R_result)
  }

  #determine which letter the constant will be
  intc = LETTERS[!LETTERS[-(1:2)]%in%all.vars(tilde)][-(1:2)][1]

  body(fun) <- Ryacas::as_r(paste0(R_result, "+", intc))
  the_formals <- formals(fun)
  the_formals[intc] <- 0

  formals(fun) <- the_formals

  fun
}

fix_names_for_yacas <- function(EX) {
  Replacements <- tibble::tribble(
    ~ R, ~ yacas,
    "cos\\(", "Cos\\(",
    "sin\\(", "Sin\\(",
    "log\\(", "Ln\\(",
    "exp\\(", "Exp\\(",
    "tan\\(", "Tan\\("
  )
  for (k in 1:nrow(Replacements))
    EX <- gsub(Replacements$R[k], Replacements$yacas[k], EX)

  EX
}

