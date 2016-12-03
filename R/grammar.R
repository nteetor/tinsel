# S => A | A S | `nil`    // I suppose one could parse an empty file
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

grammar <- function() {
  self <- new.env(parent = emptyenv())

  self$S <- function() {

  }


}
