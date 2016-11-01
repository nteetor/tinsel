timer <- function(fun) {
  force(fun)
  function() {
    el <- system.time(fun())['elapsed']
    cat('Time elapsed: ', el, '\n', sep = '')
  }
}

per_centum <- function(fun) {
  force(fun)
  function() {
    n <- fun()
    cat(n * 100, '%\n', sep = '')
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

#. per_centum
h <- function() {
  0.30 * 1.01
}
