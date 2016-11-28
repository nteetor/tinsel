# A decorator which will ramp up a color scale, expects `f()` to return colors
# to interpolate.
ramp_to <- function(f, n) {
  function(...) {
    colorRampPalette(f(...))(n)
  }
}

as_double <- function(f) {
  function(...) {
    vapply(f(...), as.double, double(1), USE.NAMES = FALSE)
  }
}
