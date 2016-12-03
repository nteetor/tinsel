is.parser <- function(x) inherits(x, 'parser')

parser <- function(file) {
  self <- new.env(parent = emptyenv())

  class(self) <- 'parser'
  self
}
