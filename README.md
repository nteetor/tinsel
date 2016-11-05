# tinsel

Decorating functions in R.

![Travis-CI Build Status](https://travis-ci.org/nteetor/tinsel.svg?branch=master)

The tinsel package adds a roxygen-flavored decorator to R. Simply put, 
decorators are a means of building upon the functionality of a function (boo...)
*f* without modifying the code for *f*. The benefit of decorators is more
clearly illustrated using object classes.

#### What're decorators all about?

Say we develop a *Spaceship* class. In addition to our standard *Spaceship*
class, we also need a class for a spaceship with a hyperdrive. So we develop a 
*SpaceshipWithHyperdrive* class. Hoever, given all our spaceships we also need a
mothership. So we develop a *MotherSpaceship*. But, what if the mothership also 
has a hyperdrive? In this have to add two new classes, *MotherSpaceship* and 
*MotherSpaceshipWithHyperdrive*. While the situation is not unmanageable adding 
a new class for each spaceship feature, or even for every couple of features, is
less than ideal.

Instead we can create a decorator for each new spaceship feature. Let's call 
these decorators *HyperdriveSpaceshipDecorator* and *MotherSpaceshipDecorator*. 
The *HyperdriveSpaceshipDecorator* takes the *Spaceship* class, with all the 
*Spaceship* methods, and adds hyperdrive-related methods. Thus, we are saved the
trouble of copying over the *Spaceship* methods to a new class as would have
been necessary to create the *SpaceshipWithHyperdrive*, *MotherSpaceship*,
and *MotherSpaceshipWithHyperdrive* classes.

#### R Decoraters (and your decorators!)

Let's create a function `if_error` which wraps a function `f` such that if `f()`
would generate an error a default value is instead returned, otherwise `f()` is 
returned.

```R
if_error <- function(f) {
  force(f)
  function(default) {
    tryCatch(
      f(),
      error = function(e) {
        message('Caught error, returning default ', default)
        return(default)
      }
    )
  }
}
```

Below we make use of our new function. We'll decorate the default `mean`
function, so instead of generating an error the function returns `Inf`. Great!

```R
mean_inf <- if_error(mean, Inf)

# give it a try!
mean_inf(1:5)
mean_inf(c(1, 'two', 3))
```

Now we'll see where the roxygen-flavoring comes in. The above code can be 
rewritten in the following format,

```R
#. if_error(Inf)
mean_inf <- mean

# please give this one a try too
mean_inf(c(30, 30))
mean_inf(c('deltron', 30, 30))
```

The special comment `#.` is used to denote a decorator for a function. In this
case, denotes `if_error` as a decorator of the function `mean`. `mean` is passed
as the first argument to `if_error` and `Inf` is passed as the second argument.
The result of decorating `mean` with `if_error` is assigned to `mean_inf`.

### Installing tinsel

You can install this package using devtools, but please be aware the package is
still in development.

```R
# install.packages('devtools')
devtools::install_github('nteetor/tinsel')
```

Check out the `source_decorated` function to get started.

---

Cheers, Nate.
