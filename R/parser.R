is.parser <- function(x) inherits(x, 'parser')

parser <- function(file) {
  self <- new.env(parent = emptyenv())
  self$scanner <- scanner(file)
  self$tokens <- stack(rev(self$scanner$tokenize()))
  self$stack <- stack()

  self$expect <- function(symbol) {
    t <- self$stack$pop()
    if (type(t) != type) {
      stop(expected(symbol, t$value, t$lineno), call. = FALSE)
    } else {
      t
    }
  }

  # start => S | `nil`
  # S => A | A A
  self$S <- function() {
    while (type((t <- self$tokens$peek())) == .type$TINSEL_COMMENT ||
           type(t) == .type$IDENTIFIER) {
      self$A()
    }
  }

  # A => #. B | \R
  self$A <- function() {
    t <- self$tokens$peek()
    if (type(t) == .type$TINSEL_COMMENT) {
      self$stack$push(self$tokens$pop())
      self$B()
    } else if (type(t) == .type$IDENTIFIER) {
      self$tokens$pop()
    } else {
      self$expect(.type$IDENTIFIER) # not great
    }
  }

  # B => [D] E | E
  self$B <- function() {
    t <- self$tokens$peek()
    if (type(t) == .type$FILE_REFERENCE) {
      self$D()
      self$E()
    } else if (type(t) == .type$IDENTIFIER) {
      self$E()
    } else {
      self$expect(.type$IDENTIFIER) # not great
    }
  }

  # D => `file` | `file.extension`
  self$D <- function() {
    t <- self$expect(.type$FILE_REFERENCE)

    if (!grepl('.', t$value, fixed = TRUE)) {
      t$value <- paste0(t$value, '.R')
    }
    self$stack$push(t)
  }

  # E => G | G(H)
  self$E <- function() {
    self$G()
    #self$H()  # currently not handling arguments to decorators
  }

  # G => `package`::`decorator` | `decorator`
  self$G <- function() {
    t <- self$expect(.type$IDENTIFIER)

    if (type(self$tokens$peek()) == .type$PACKAGE_ACCESSOR) {
      tpkg <- t
      # Construct new "call" node (or whatever the class gets named) with
      # reference to package
      self$expect(.type$PACKAGE_ACCESSOR)
      tcall <- self$expect(.type$IDENTIFIER)
    } else {
      self$stack$push(t)
    }
  }

  # H => I | `nil`   // For now I am not doing little but pass arguments
  #                  // to the decorator
  self$H <- function() {
    NULL
  }

  # I => \R | \R, I
  self$I <- function() {
    NULL
  }

  class(self) <- 'parser'
  self
}
