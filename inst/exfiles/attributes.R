# tinsel example file
#   @date Nov. 16, 2016
#   @author nteetor
#   @description Use decorators to assign attributes to function output.

#' Assign Attributes Decorator
#'
#' A decorator function to assign attributes, see below for more information.
#'
#' @param f A function.
#' @param \ldots Function names, specified by bare names or character strings.
#'
#' @return
#'
#' A decorator function which will assign attributes to \code{f}'s output
#' based on the function names passed to \code{attribute} as \ldots.
#'
attribute <- function(f, ...) {
  funs <- eval(substitute(alist(...)))
  function(...) {
    fret <- f(...)
    for (fun in funs) {
      attr(fret, as.character(fun)) <- do.call(eval(fun), list(fret))
    }
    fret
  }
}

#. attribute(mean, 'sum')
selector1 <- function(.data, column, rows) {
  if (missing(rows)) rows <- seq_len(NROW(.data))
  .data[rows, column]
}

# The selector2 function below returns the same result as selector1, but uses a
# more pedantic, sometimes preferable, style of decorating.
# (note that the order of the attribute is different, but the values are the
# same)

#. attribute(mean)
#. attribute(sum)
selector2 <- function(.data, column, rows) {
  if (missing(rows)) rows <- seq_len(NROW(.data))
  .data[rows, column]
}
