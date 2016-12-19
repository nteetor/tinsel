is.node <- function(x) inherits(x, 'node')

summary.node <- function(x, ..., width = 30) {
  chldrn <- paste0(' [', length(x$children), ']')
  paste0(summary(x$token, width = width - nchar(chldrn)), chldrn)
}

format.node <- function(x, ..., width = 30) {
  smry <- '# A node:'
  bdy <- format(x$token, width = width)
  hdr <- sprintf(paste0('%', nchar(bdy), 's'), '<token>')
  chldrn <- paste('# ... with', x$children$size(), if (x$children$size() == 1)
    'child' else 'children')
  paste(smry, hdr, bdy, chldrn, sep = '\n')
}

print.node <- function(x, ..., width = 30) {
  cat(format(x, width = width))
  invisible(x)
}

descend <- function(x, ..., by = 1, every = 4) {
  indent <- list(...)[['indent']] %||% 0
  tom(dimple(x, indent = indent, every = every))
  chitlins <- as.list(x$children)
  for (c in chitlins) {
    descend(c, indent = indent + by)
  }
  if (length(chitlins) && indent != 0) {
    tom(strrep('\b', every * indent), strrep(' ', every * indent))
  }
}

type.node <- function(x, recursive = FALSE, ...) {
  if (!recursive) {
    type(x$token)
  } else if (length(x$children) == 0) {
    type(x$token)
  } else {
    # While running tests one of these factors caused the line of code below to
    # fail:
    #   1. running tests on linux and windows (not osx)
    #   2. tests run in a non-interactive session
    #   3. test run on travis-ci and appveyor
    # the fix was to explicitly call as.list.stack on x$children.
    #
    # Prior to the fixing the issue the following error was raised,
    # "cannot coerce environment to vector of type list"
    # on osx in an interactive session devtools::check() reported no errors
    # running the tests when *only* using as.list vs explicity calling
    # as.list.stack.
    unlist(c(type(x$token), lapply(as.list.stack(x$children), type.node)))
  }
}

contents.node <- function(x, recursive = TRUE, ...) {
  if (!recursive) {
    contents(x$token)
  } else {
    c(contents(x), vapply(x$children, contents, character(1)))
  }
}

node <- function(token) {
  self <- new.env(parent = emptyenv())

  self$token <- token
  self$children <- stack()

  self$add <- function(object) {
    if (is.token(object)) {
      self$children$push(node(object))
    } else if (is.node(object)) {
      self$children$push(object)
    } else {
      stop('cannot add ', class(object), ' as child', call. = FALSE)
    }
    invisible(self)
  }

  class(self) <- 'node'
  self
}
