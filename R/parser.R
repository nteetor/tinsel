is.parser <- function(x) inherits(x, 'parser')

parser <- function(file) {
  self <- new.env(parent = emptyenv())
  self$scanner <- scanner(file)
  self$tokens <- stack(self$scanner$tokenize())

  # start => S | `nil`    // I suppose one could parse an empty file
  # S => A | A A
  self$S <- function() {
    t <- self$tokens$peek()
    if (type(t) == .type$TINSEL_COMMENT) {

    } else if (type(t) == .type$IDENTIFIER) {

    }
  }
  #
  # A => #. B | \R   // \R an R expression (including comments)
  #
  # B => [D] E | E
  #
  # D => `file` | `file.extension`
  #
  # E => F | F(G)
  #
  # F => `package`::`decorator` | `decorator`
  #
  # G => H | `nil`   // For now I am not doing little but pass arguments
  #                  // to the decorator
  #
  # H => \R | \R, H
  #

  class(self) <- 'parser'
  self
}
