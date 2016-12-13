add1 <- function(f) {
  function() {
    f() + 1
  }
}

subtract1 <- function(g) {
  function() {
    g() - 1
  }
}

#. add1
#. subtract1
addition <- function(a, b) {
  a + b
}
