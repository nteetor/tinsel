expect_char <- function(.traversal, char) {
  eval(bquote(expect_equal(.(.traversal$getchar()), .(char))))
}

expect_line <- function(.traversal, string) {
  lign <- .traversal$getline()
  if (length(lign) != nchar(string)) {
    msg <- sprintf('length(c(%s)) != nchar(%s)', paste(lign, collapse = ', '),
                   string)
    fail(msg)
  } else if (paste(lign, collapse = '') != string) {
    msg <- sprintf('all(c(%s) != "%s")', paste(lign, collapse = ', '), string)
    fail(msg)
  } else {
    eval(bquote(expect_equal(.(lign), .(re_split(string, '')))))
  }
}

expect_has_fields <- function(class, ...) {
  expect_has_elements(class, Negate(is.function), 'fields', ...)
}

expect_has_methods <- function(class, ...) {
  expect_has_elements(class, is.function, 'methods', ...)
}

expect_has_elements <- function(class, predicate, label = NULL, ...) {
  pred <- substitute(predicate)
  predfunct <- eval(pred)
  predname <- deparse(pred)
  methods <- vapply(list(...), `[[`, character(1), 1)
  celements <- ls(class, all.names = TRUE)
  cmethods <- sort(Filter(function(e) predfunct(class[[e]]), celements))
  methods <- sort(methods)

  if (length(cmethods) != length(methods) || any(cmethods != methods)) {
    if (is.null(label)) {
      predname <- sub('^is\\.', '', predname)
      if (nchar(predname > 13)) {
        predname <- paste0(substr(predname, 1, 12), '...')
      }

      label <- paste(predname, 'elements')
    }

    classname <- class(class)[1]
    title <- sprintf('%s for class %s,\n', label, classname)
    expected <- 'Expected'
    found <- 'Found'

    master <- sort(unique(c(cmethods, methods)))
    cmethods <- as.list(set_names(cmethods))
    methods <- as.list(set_names(methods))

    width <- max(nchar(master))

#    check <- '\u2713'
    check <- function(n) if (!is.null(n)) '*' else ''
    template <- paste0('|%-', width, 's |%-', nchar(found), 's |%-', nchar(expected), 's\n')
    msg <- c(title, sprintf(template, 'Names', found, expected))

    for (m in master) {
      msg <- c(msg, sprintf(template, m, check(cmethods[[m]]), check(methods[[m]])))
    }

    msg <- paste(msg, collapse = '', sep = '')
    fail(msg)
  } else {
    succeed('object is in order')
  }
}
