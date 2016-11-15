#' Decorated Functions
#'
#' Check if a function is decorated.
#'
#' @param f A function.
#'
#' @return
#'
#' Returns \code{TRUE} or \code{FALSE} depending on if the object \code{f} is
#' decorated.
#'
#' @export
is.decorated <- function(f) {
  inherits(f, 'decorated')
}

#' Print a Decorated Function
#'
#' The \code{print.decorated} function naively prints \code{x} as a function. In
#' reality, the function printed may be the final of any number of decorators to
#' a decoratee. To get the original function or the decorators wrapping it use
#' \code{\link{original}} and \code{\link{decorators}}.
#'
#' @param x A decorated function.
#' @param \ldots Additional arguments for next print method.
#'
#' @export
print.decorated <- function(x, ...) {
  NextMethod('print')
}

#' Get Function Decorators or Original Function
#'
#' These functions allow programmers to access the decorators of a function or
#' the original decoratee function.
#'
#' @param f A decorated function.
#'
#' @name decorators
#' @export
decorators <- function(f) {
  stopifnot(is.function(f), is.decorated(f))

  attr(f, 'decorators', exact = TRUE)
}

#' @rdname decorators
#' @export
original <- function(f) {
  stopifnot(is.function(f), is.decorated(f))

  attr(f, 'decoratee', exact = TRUE)
}
