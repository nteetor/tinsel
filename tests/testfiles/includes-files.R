#. [sample-utils] ramp_to(5)
div_scale <- function() {
  cm.colors(3)
}

#. [sample-utils] as_double
dbl_c <- `c`

#. [r-function.utils] stats::runif
#.   [sample-utils.R] as_double
randoms <- function(m) {
  m * 10
}

#. [r-function.utils] draw_box
smiley <- function() {
  expr <- ':^)'
  cat(noquote(expr))
}
