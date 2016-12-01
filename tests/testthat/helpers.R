`%has_method%` <- function(o, m) {
  exists(m, envir = o, inherits = FALSE)
}

expect_char <- function(traversal, char) {
  eval(bquote(expect_equal(.(traversal$getchar()), .(char))))
}

expect_line <- function(traversal, string) {
  lign <- traversal$getline()
  if (length(lign) != nchar(string)) {
    msg <- sprintf('length(c(%s)) != nchar(%s)', paste(lign, collapse = ', '),
                   string)
    return(fail(msg))
  }
  if (paste(lign, collapse = '') != string) {
    msg <- sprintf('all(c(%s) != "%s")', paste(lign, collapse = ', '), string)
    return(fail(msg))
  }
  eval(bquote(expect_equal(.(lign), .(re_split(string, '')))))
}

expect_has_methods <- function(class, ...) {
#  print(eval(substitute(alist(...))))
  methods <- vapply(eval(substitute(alist(...))), deparse, character(1))
  celements <- ls(class, all.names = TRUE)
  cmethods <- sort(Filter(function(e) is.function(class[[e]]), celements))
  methods <- sort(methods)

  if (length(cmethods) != length(methods) || all(cmethods != methods)) {
    title <- 'Methods found did not match expected\n'
    expected <- 'Expected'
    classname <- class(class)[1]

    master <- sort(unique(c(cmethods, methods)))
    cmethods <- as.list(set_names(cmethods))
    methods <- as.list(set_names(methods))

    maxname <- max(nchar(master))
    width <- maxname
    if (width + nchar(classname) + nchar(expected) + 5 > nchar(title)) {
      title <- sprintf(
        paste0(
          '%',
          nchar(title) +
            (width + nchar(classname) + nchar(expected) + 5) - nchar(title),
          's'),
        title
      )
      print(title)
    }

#    check <- '\u2713'
    check <- 'X'

    template <- paste0('%', width, 's  %', nchar(classname), 's  %', nchar(expected), 's\n')
    print(template)
    msg <- c(title, sprintf(template, 'Names', classname, expected))

    for (m in master) {
      msg <- c(
        msg,
        sprintf(template, m, cmethods[[m]] %!!% check, methods[[m]] %!!% check)
      )
    }

    msg <- paste(msg, collapse = '', sep = '')
    fail(msg)
  }
}
