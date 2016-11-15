log_to <- function(f, path = '.log') {
  function(...) {
    lcon <- file(path, "w")
    on.exit(close(lcon))
    fval <- f(...)
    cat(fval, '\n', file = lcon)
    fval
  }
}

#. log_to(path = '.log.whoops')
whoops <- function(x, ) {
  if (!y) return('why not?')

  'Did I miss something?'
}
