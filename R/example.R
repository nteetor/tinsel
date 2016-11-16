#' Get Path of a Tinsel Example File
#'
#' \code{tinsel_example} simplifies getting and returns the system path of an
#' example file included in the tinsel package. To list the names of all example
#' files use \code{tinsel_examples}.
#'
#' @param path Name of the example file.
#'
#' @keywords internal
#' @export
#' @examples
#' # list all example files
#' tinsel_examples()
#'
#' # get the path of a specific example
#' tinsel_example('attributes.R')
tinsel_example <- function(path) {
  system.file('exfiles', path, package = 'tinsel', mustWork = TRUE)
}

#' @rdname tinsel_example
#' @export
tinsel_examples <- function() {
  dir(system.file('exfiles', package = 'tinsel', mustWork = TRUE))
}
