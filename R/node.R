is.node <- function(x) inherits(x, 'node')

as.character.node <- function(x, ...) {
  paste(as.character(x$token), sprintf('[%d]', length(x$children)))
}

print.node <- function(x, ...) {
  smry <- '# A node:'
  chtok <- as.character(x$token)
  hdr <- sprintf(paste0('%', nchar(chtok), 's'), paste0('<', class(x$token), '>'))
  bdy <- paste(hdr, as.character(x$token), sep = '\n')
  chldrn <- paste('# ... with', x$children$size(), if (x$children$size() == 1)
                 'child' else 'children')
  tom(smry)
  tom(bdy)
  if (length(x$children)) tom(chldrn)
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
    # while running tests one of these factors caused the following line to
    # fail:
    #   1. runing on linux and windows (not osx)
    #   2. non-interactive session
    #   3. travis-ci and appveyor
    # the fix was to explicitly call as.list.stack on x$children prior to the
    # fix the following error was raised,
    # "cannot coerce environment to vector of type list"
    # on osx in an interactive session devtools::check() reported no errors
    # running the tests when *only* using as.list vs the explicity
    # as.list.stack
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
