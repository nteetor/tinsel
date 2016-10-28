# tinsel

Decorate functions in R

Inspired by Python decorators the tinsel package aims to bring roxygen-flavored
decorators to R. See below,

```R 
#. per_centum 
calculations <- function(a, b) { 
  a * b 
} 
```

This function would be equivalent to `per_centum(calculations(a, b))`. Let's say
`per_centum` is naively defined as,

```R
per_centum <- function(n) {
  paste0(n * 100, '%')
}
```

tinsel helps create the final `calculations` function, decorated to its fullest,
by parsing the source file, identifying decorators denoted by "#.", and loading
a bundled `calculations` function into the calling environment (or a specified
environment).

There's pleny of work yet to do, so please bear with me. I hope the end
product is a blast. Cheers!
