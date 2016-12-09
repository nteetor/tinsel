`%||%` <- function(a, b) if (is.null(a)) b else a

tom <- function(...) cat(..., '\n', sep = '')

dimple <- function(x, ..., indent = 0, every = 2) {
  bar <- sprintf(paste0('%-', every, 's'), '|')
  bars <- strrep(bar, indent)
  paste0(
    paste0(bars, paste0('|', capture.output(print(x, ...))), collapse = '\n'),
    '\n',
    paste0(bars, bar)
  )
}

trunk <- function(.string, n, suffix = '..') {
  if (nchar(.string) > n) {
    paste0(substr(.string, 1, n - nchar(suffix)), suffix)
  } else {
    .string
  }
}

char_at <- function(.string, i) {
  if (abs(i) > nchar(.string) || i == 0) stop('index out of bounds', call. = FALSE)
  j <- (i %% nchar(.string)) + (i < 0)
  substr(.string, j, j)
}

first_of <- function(.string, c) {
  if (!grepl(c, .string, fixed = TRUE)) return(-1)
  i <- 1
  while (i < nchar(.string) && char_at(.string, i) != c) i <- i + 1
  i
}

re_split <- function(.string, pattern) {
  strsplit(.string, pattern, perl = TRUE)[[1]]
}

re_search <- function(.string, pattern) {
  bounds <- regexpr(pattern, .string)
  if (length(bounds) && bounds[[1]] != -1) {
    substr(.string, bounds[[1]], bounds[[1]] + attr(bounds, 'match.length') - 1)
  } else {
    NULL
  }
}

re_match <- function(.string, pattern) {
  if (nchar(pattern) == 1) {
    grepl(pattern, .string, fixed = TRUE)
  } else {
    grepl(paste0('^', pattern, '$'), .string, perl = TRUE)
  }
}

set_names <- function(obj, nm) {
  if (missing(nm)) nm <- vapply(obj, as.character, character(1))
  names(obj) <- nm
  obj
}

is.connection <- function(x) inherits(x, 'connection')
