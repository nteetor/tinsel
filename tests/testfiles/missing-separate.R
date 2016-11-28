#. whoops$fizz(5)
razz <- function(n) {
  for (i in seq_len(n)) {
    cat(strrep(' ', i - 1), 'fizz\n', sep = '')
  }
}
