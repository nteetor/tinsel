# tinsel

Decorating functions in R.

![Travis-CI Build Status](https://travis-ci.org/nteetor/tinsel.svg?branch=master) [![codecov](https://codecov.io/gh/nteetor/tinsel/branch/master/graph/badge.svg)](https://codecov.io/gh/nteetor/tinsel) ![cran status](http://www.r-pkg.org/badges/version/tinsel)


The tinsel package adds function decorators to R using a special `#.` comment. 
Decorators are a means of transforming a function without needing to rewrite 
the function. They allow for easy integration of new code onto existing code. These benefits
are illustrated below with an example about object classes.

### What are decorators all about?

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

### R Decorators (and *your* decorators)

Let's create a function `if_warning` which wraps a function `f` such that if `f(...)`
would generate a warning a default value is returned instead, otherwise `f(...)` is 
returned.

```R
if_warning <- function(f, default) {
  function(...) {
    tryCatch(
      f(...),
      warning = function(w) {
        default
      })
  }
}
```

Now let's transform the default `mean` function, so instead of generating a 
warning the function returns `Inf`. Great!

```R
mean_inf <- if_warning(mean, Inf)

# give it a try!
mean_inf(1:5)
mean_inf(c(1, 'two', 3))
```

Here is where the special comment `#.` comes in. The above code can be 
rewritten as the following,

```R
#. if_warning(Inf)
mean_inf <- mean
```

The special comment `#.` is used to denote a function decorator. In this 
example, `#. if_warning` denotes `if_warning` as the decorator of the decoratee
`mean`. `mean` is passed as the first argument to `if_warning` and `Inf`
as the second argument. The result of transforming `mean` with `if_warning` is
assigned to `mean_inf`.

In order to see the decorator annotation example in action, save the above code 
in a file, source the file using `source_decoratees`, and then call `mean_inf` 
once more. (Gentle reminder, the output is expected to be the same)

### Installing tinsel

tinsel is now available on CRAN.

```R
install.packages('tinsel')
```

You can install the latest version of tinsel using devtools (currently even with version 0.0.1 on CRAN).

```R
# install.packages('devtools')
devtools::install_github('nteetor/tinsel')
```

Check out the `source_decoratees` function to get started. 

### RStudio Addin

If you are working in RStudio the tinsel package includes an addin for the core 
function `source_decoratees`. To bind the addin to a keyboard shortcut in
RStudio navigate to **Tools** > **Addins** > **Browse Addins** > **Keyboard 
Shorcuts**. For more information about the keyboard shortcuts checkout the 
RStudio [support 
page](https://support.rstudio.com/hc/en-us/articles/206382178-Customizing-Keyboard-Shortcuts).
If you choose to setup a keyboard shortcut for the addin I recommend
<kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>S</kbd> since 
<kbd>Cmd</kbd>+<kbd>Shift</kbd>+<kbd>S</kbd> (or 
<kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>S</kbd> on Linux and Windows) is the source active file 
shortcut. The end result is you can quickly load your decorated functions like
you would source all functions from the active file.

---

Cheers, Nate.
