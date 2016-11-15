excite <- function(.f, level = 1) {
  stopifnot(is.numeric(level))
  function(...) {
    val <- .f(...)
    paste0(val, strrep('!', level))
  }
}

#. excite(5)
my_name <- function(x) {
  rep('Nathan Teetor', times = x)
}

#. excite(level = 10)
too_energetic <- function(x) {
  rep('Nathan Teetor', times = x)
}
