timer <- function(fun) {
  function(...) {
    el <- system.time(fval <- fun(...))['elapsed']
    cat('Time elapsed: ~ ', el, ' seconds\n', sep = '')
    fval
  }
}

per_centum <- function(fun) {
  function() {
    paste0(round(fun() * 100, 2), '%')
  }
}

#. timer
fib <- function(n) {
  round(((1.61805 ** (1:n - 1)) + (1.61805 ** (1:n - 2))) / sqrt(5))
}

#' An Undecorated Function
#'
#' This function is ignored by \link{source_decoratees}.
#'
#' @return
#'
#' This function throws an error.
#'
#' @export
ignore <- function() {
  stop('I was not ignored.', call. = FALSE)
}

#. timer
#. per_centum
progress <- function() {
  #. timer
  len <- 20
  msgs <- vapply(
    seq_len(len),
    function(l) paste0('[', strrep('=', l), strrep(' ', len - l), ']'),
    character(1)
  )

  cat(msgs[1], sep = '')
  for (m in msgs[-1]) {
    Sys.sleep(0.1)
    if (runif(1) <= 0.25) break
    cat(strrep('\b', nchar(m)), m, sep = '')
  }
  cat('\n')

  which(m == msgs) / length(msgs)
}

if_warning <- function(f, default) {
  function(...) {
    tryCatch(
      f(...),
      warning = function(e) {
        default
      })
  }
}

#. if_warning(Inf)
mean_inf <- mean

#. if_warning('whoops!')
one_fish <-
  function(two_fish = NULL) {
    'red fish, blue fish'
  }

# emphasize text
emph <- function(f, begin = '**', end = begin) {
  function(...) {
    paste(begin, f(...), end)
  }
}

#. emph
my_name <- function() 'Nathan Teetor'

#. emph('<b>', '</b>')
cats <- function(n) {
  paste(rep('cats', n), collapse = ' ')
}
