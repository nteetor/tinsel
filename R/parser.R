is.parser <- function(x) inherits(x, 'parser')

parser <- function(file) {
  self <- new.env(parent = emptyenv())
  self$scanner <- scanner(file)
  # TODO fix rev() workaround
  self$tokens <- stack(rev(as.list(self$scanner$tokenize())))
  self$table <- {
    src <- new.env(parent = baseenv())
    source(file, local = src, keep.source = FALSE, verbose = FALSE)
    src
  }
  self$stack <- stack()
  self$tree <- node(token('SOF', -1, 1))

  self$expect <- function(symbol) {
    if (type(self$tokens$peek()) != symbol) {
      stop(expected(symbol, t$value, t$lineno))
    }
    self$tokens$pop()
  }

  # start => S | `nil`
  self$parse <- function() {
    self$S()
    self$tree$tolist()
  }

  # S => A | A A
  self$S <- function() {
    while (type(self$tokens$peek()) != .type$EOF) {
      self$A()
    }
  }

  # A => #. B Z | \R
  self$A <- function() {
    t <- self$tokens$pop()
    if (type(t) == .type$TINSEL_COMMENT) {
      self$stack$push(t)
      self$B()
      self$Z()
    }
  }

  # Z => `variable name`    // looked up in self$table
  self$Z <- function() {
    t <- self$expect(.type$IDENTIFIER)
    if (!exists(value(t), self$table, inherits = FALSE)) {
      stop('on line ', t$lineno, ', no definition found for ', value(t),
           call. = FALSE)
    }
    self$stack$push(t)
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

    self$tree$push(t)
  }

  # E => G | G(H)
  self$E <- function() {
    self$G()
    #self$H()  # currently not handling arguments to decorators
  }

  # G => `package`::`decorator` | `decorator`
  self$G <- function() {
    t <- self$expect(.type$IDENTIFIER)
    b <- node(t)

    if (type(self$tokens$peek()) == .type$PACKAGE_ACCESSOR) {
      tpkg <- t
      # Construct new "call" node (or whatever the class gets named) with
      # reference to package
      newb <- b
      type(newb) <- .type$PACKAGE_NAME
      b <- node(self$expect(b))
      acc <- self$expect(.type$PACKAGE_ACCESSOR)
      tcall <- self$expect(.type$IDENTIFIER)

      self$tree$push(acc)
      self$tree$push(tcall)
      self$tree$push(tpkg)
    } else {
      self$tree$push(t)
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
