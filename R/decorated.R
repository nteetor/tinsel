#' Decorators
#'
#' Test if an object is decorated.
#'
#' @param x An \R object.
#'
#' @name decorated
#' @export
is.decorated <- function(x) inherits(x, 'decorated')

#' Print Decorated Functions
#'
#' A wrapper on top of the standard \code{\link{print.function}} to help make
#' better sense of decorated functions. Possible arguments are those arguments
#' which are used by the decoratee, but are not explicitly listed as part of the
#' final transformed funciton.
#'
#' @param x An \R object.
#' @param \ldots Additional arguments passed on to methods.
#'
#' @export
print.decorated <- function(x, ...) {
  cat('Possible arguments:\n')
  formaltees <- formals(attr(x, 'decoratee', exact = TRUE))
  for (arg in names(formaltees)) {
    if (!is.null(formaltees[[arg]])) {
      val <- paste0(' = ', formaltees[[arg]])
    } else {
      val <- ''
    }
    cat0('  ', arg, val, '\n')
  }
  print(body(x))
}

#' List Decorators
#'
#' A helper function to list the decorators of a function.
#'
#' @param f A decorated function.
#'
#' @export
decorators <- function(f) {
  decos <- attr(f, 'decorators', exact = TRUE)
  if (is.null(decos)) {
    message('argument `f` is not decorated', call. = FALSE)
  } else {
    for (d in names(decos)) {
      cat0(which(d == names(decos)), '. ', d, '\n')
      print(decos[[d]])
    }
  }
}
