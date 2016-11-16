#' Get Function Decorators or Original Function
#'
#' Get the decorators of a function or the original decoratee function from a
#' decorated function object.
#'
#' @param f A decorated function.
#'
#' @name decorators
#' @export
#' @examples
#' source_decoratees(tinsel_example('attributes.R'))
#'
#' # sourced from the 'attributes.R' example file
#' selector1
#'
#' # get a list of decorators wrapping a function
#' decorators(selector1)
#'
decorators <- function(f) {
  stopifnot(is.function(f), is.decorated(f))

  attr(f, 'decorators', exact = TRUE)
}

#' @rdname decorators
#' @export
#' @examples
#' # get the original decoratee function of the
#' # decorated `selector1` function
#' original(selector1)
#'
original <- function(f) {
  stopifnot(is.function(f), is.decorated(f))

  attr(f, 'decoratee', exact = TRUE)
}
