## The only way to turn an object into a stagerunner is if it can be
## interpreted as a hierarchical sequence of execution: run some stuff
## in group A, then run some stuff in group B, and so on, with each
## group potentially containing more subgroups.
##
## In other words, the things which can be turned into stagerunners
## are:
## 
##    * functions
##    * other stagerunners (sub-stagerunners)
##    * lists composed of the above
##
## The purpose of the `is_pre_stagerunner` function is to determine
## whether an object satisfies these restrictions.
#' Whether or not an object can be transformed into a stageRunner.
#'
#' @param x ANY. An R object for which it will be determined whether or not
#'    it admits a conversion to a stageRunner.
#' @return TRUE or FALSE according to whether the object can be transformed
#'    to a stageRunner. In general, only a function or list of functions
#'    can be turned into a stageRunner.
#' @export
#' @examples
#' stopifnot(is_pre_stagerunner(function(e) { e$x <- 1 }))
#' stopifnot(is_pre_stagerunner(list(function(e) { e$x <- 1 }, function(e) { e$y <- 2 })))
#' stopifnot(is_pre_stagerunner(
#'   list(a = function(e) { e$x <- 1 },
#'     list(b = function(e) { e$y <- 2 }, c = function(e) { e$z <- 3 }))))
#' 
#' stopifnot(!is_pre_stagerunner(NULL))
#' stopifnot(!is_pre_stagerunner(5))
#' stopifnot(!is_pre_stagerunner(iris))
is_pre_stagerunner <- function(x) {
  if (is.function(x) || is.stagerunner(x)) { return(TRUE) }
  if (!is.recursive(x) || is.environment(x)) { return(FALSE) }

  ## Using a for loop is a tiny bit faster than a apply-family operation
  ## because we can exit the function early.
  for (i in seq_along(x)) {
    ## We use the base function [`Recall`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Recall.html)
    ## for its recursive effect.
    if (!(is.function(x[[i]]) || is.stagerunner(x[[i]]) || is.null(x[[i]]) ||
          (is.recursive(x[[i]]) && Recall(x[[i]])))) {
      return(FALSE)
    }
  }

  TRUE
}

