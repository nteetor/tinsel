#' Decorated Functions
#'
#' Returns \code{TRUE} if the function \code{f} is decorated, otherwise
#' \code{FALSE}.
#'
#' @param f A function.
#'
#' @return
#'
#' \code{TRUE} or \code{FALSE}.
#'
#' @export
#' @examples
#' source_decoratees(tinsel_example('timer.R'))
#'
#' # sourced from the timer.R example file
#' is.decorated(waldo)
#' is.decorated(jack)
#'
#' # it's a function, but not decorated
#' is.decorated(mean)
#'
#' # far from the mark
#' is.decorated(3030)
#'
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
#' @param \ldots Additional arguments for next \code{print} method.
#'
#' @export
#' @examples
#' source_decoratees(tinsel_example('tags.R'))
#'
#' print(html_paragraph)
#' print(html_bold)
#'
print.decorated <- function(x, ...) {
  attributes(x) <- NULL
  NextMethod('print', object = x, ...)
}
