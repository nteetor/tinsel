aget <- function(envir = as.environment(-1), mode = 'any', inherits = FALSE,
                 ifnotfound = NULL) {
  allvars <-
    Filter(
      Negate(is.null),
      sapply(
        ls(envir = envir, all.names = TRUE, sorted = FALSE),
        function(name) {
          get0(name, envir = envir, mode = mode, inherits = inherits,
               ifnotfound = ifnotfound)
        },
        simplify = FALSE,
        USE.NAMES = TRUE
      )
    )
  if (!length(allvars)) {
    return(list())
  }
  allvars
}

clone <- function(x) {
  if (!is.environment(x)) {
    return(x)
  }
  if (!length(aget(x, mode = 'environment'))) {
    cx <- list2env(as.list.environment(x), parent = emptyenv())
    class(cx) <- class(x)
    cx
  }
  lx <- list2env(lapply(as.list.environment(x), clone))
  class(lx) <- class(x)
  lx
}
