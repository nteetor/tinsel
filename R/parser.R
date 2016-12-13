is.parser <- function(x) inherits(x, 'parser')

parser <- function(file) {
  self <- new.env(parent = emptyenv())
  self$scanner <- scanner(file)
  self$tokens <- stack() # stack of a stack reverses
  self$lookup <- {
    src <- new.env(parent = baseenv())
    source(file, local = src, keep.source = FALSE, verbose = FALSE)
    src
  }
  self$stack <- stack()
  self$tree <- NULL

  self$expect <- function(symbol) {
    t <- self$tokens$pop()
    if (type(t) != symbol) {
      stop(expected(symbol, contents(t), t$lineno))
    }
    t
  }

  self$shift <- function() {
    self$stack$push(node(self$tokens$pop()))
  }

  self$reduce <- function(symbols, to) {

  }

  # start => S | `nil`
  self$parse <- function() {
    self$tokens <- clone(self$scanner$tokenize())
    self$stack <- stack()
    self$tree <- node(token('', .type$SOF, 1))

    self$S()
    self$tree
  }

  # S => A | S A
  self$S <- function() {
    while (type(self$tokens$peek()) != .type$EOF) {
      self$A()
    }
  }

  # A => B | C
  self$A <- function() {
    self$B()
    self$C()
  }

  # B => D E | E
  self$B <- function() {
    self$D()
    self$E()
  }

  # C => `identifier`
  self$C <- function() {
    t <- self$expect(.type$IDENTIFIER)
    n <- self$stack$pop()
    n$add(t)
    self$stack$push(n)
  }

  # D => G | G G
  self$D <- function() {

  }

  # E => `R expression`
  self$E <- function() {
    t <- self$expect(.type$IDENTIFIER) # | STRING | NUMBER
    n <- self$stack$pop()
    n$add(t)
    self$stack$push(n)
  }

  # G => H [I] J | H J
  self$G <- function() {
    self$H()
    self$I()
    self$J()
  }

  # H => #.
  self$H <- function() {
    if (type(self$tokens$peek()) == .type$TINSEL_COMMENT) {
      t <- self$expect(.type$TINSEL_COMMENT)
      if (type(self$stack$peek()) == .type$TINSEL_EXPR) {
        n <- node(token('', .type$DECORATORS, t$lineno))
        self$stack$push(n)
      } else if (type(self$stack$peek()) == .type$DECORATORS) {
#        n <- self
      }
    }
  }

  # I => \R | \R, I
  self$I <- function() {
    NULL
  }

  class(self) <- 'parser'
  self
}
