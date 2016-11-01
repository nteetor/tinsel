# tinsel

Decorate functions in R

[![Travis-CI Build Status](https://travis-ci.org/nteetor/tinsel.svg?branch=master)]

Inspired by Python decorators the tinsel package adds roxygen-flavored 
decorator capabilities to R. See below,

```R 
#. per_centum 
calculations <- function(a, b) { 
  a * b 
} 
```

This function is equivalent to `per_centum(calculations(a, b))`, where
`per_centum` is naively defined as,

```R
per_centum <- function(f) {
  force(f)
  function(a, b) {
    paste0(f(a, b) * 100, '%')
  }
}
```

tinsel helps create the final `calculations` function, decorated to its fullest,
by parsing the source file, identifying decorators denoted by "#.", and loading 
the final `calculations` function into a specified environment.

You can install the development version of this package using devtools,

```R
# install.packages('devtools')
devtools::install_github('nteetor/tinsel')
```

Check out the `presents` function to get started.

---

There's pleny of work yet to do, so please bear with me. I hope the end
product is a blast. Cheers, Nate.
