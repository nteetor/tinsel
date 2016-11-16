# tinsel example file
#   @date Nov. 16, 2016
#   @author nteetor
#   @description Use decorators to time functions.

#' Timer Decorator
#'
#' A decorator function to time the evaluation of a function.
#'
#' @param f A function.
#'
#' @return
#'
#' Invisibly returns the value of calling \code{f}.
#'
timer <- function(f) {
  function(...) {
    ptime <- system.time(fret <- f(...))
    cat('~', ptime[['elapsed']], 'seconds elapsed\n')
    invisible(fret)
  }
}

#. timer
waldo <- function(n) {
  Dots <- runif(n)
  windex <- sample(n, 1)
  plot(Dots)
  points(windex, Dots[windex], col = 'red')
  "Where's Waldo?"
}

#. timer
jack <- function() {
  iters <- seq_len(sample(10:20, 1))

  for (i in iters) {
    Sys.sleep(0.75)
    cat(noquote('All work and no play makes Jack a dull boy,'), '\n')
  }
}
