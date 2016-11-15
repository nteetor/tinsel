is_character <- function(f) {
  function(..., msg = '[\u2713] function output is character') {
    c <- f(...)
    if (is.character(c))
      cat(msg, '\n')
    else
      stop('found object of class', class(c), call. = FALSE)
    c
  }
}

#. is_character
hello <- function(world = 'world!') {
  paste('hello,', world)
}
