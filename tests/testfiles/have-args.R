excite <- function(f, level = 1) {
  stopifnot(is.numeric(n))
  function(s) {
    paste0(s, strrep('!', level))
  }
}

#. excite(5)
my_name <- function() {
  'Nathan Teetor'
}

#. excite(level = 10)
too_energetic <- function() {
  'Nathan Teetor'
}
