timer <- function(fun) {
  function() {
    el <- system.time(fval <- fun())['elapsed']
    cat('Time elapsed: ~', el, ' seconds\n', sep = '')
    fval
  }
}

per_centum <- function(fun) {
  function() {
    paste0(fun() * 100, '%')
  }
}

#. timer
f <- function() {
  cat('Hello, world!\n')
}

#' Roxygen!
#'
#' ignore me
#'
g <- function() {
  #. confusion
  5:1
}

#. timer
#. per_centum
h <- function() {
  0.30 * 1.01
}
