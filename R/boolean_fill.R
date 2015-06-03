## A stagerunner describes a *linear* sequence of execution: import data,
## perform this munging step, then that munging, then do some modeling, etc.
## However, it is structured hierarchically as a nested list for easier
## usability. This function will create a nested list with the exact same
## structure as the stagerunner except that each terminal node is either
## `TRUE` or `FALSE`. 
##
## Specifically, given a tree structure with exactly one `TRUE` value in the
## terminal nodes, all successors of that node will be marked as `TRUE`
## as well. Conversely, if `forward = FALSE`, then all predecessors of
## that node will be marked as `TRUE`.
## 
## For example, imagine we have a stagerunner with the following stages:
##
##   * import data
##   * clean data
##      * impute variable 1
##      * discretize variable 2
##   * train model
##
## If `el`, the first argument to the `boolean_fill` function depicted on 
## the right, refers to the "impute variable 1" stage, it will be represented
## as `el = list(F, list(T, F), F)`.
##
##   * import data
##   * clean data
##      * __impute variable 1__
##      * discretize variable 2
##   * train model
##
## Then calling the `boolean_fill` function depicted on the right with
## `forward = TRUE` will signify we want to select the stages occuring
## afterwards:
##
# The left panel will explain at a high level the code you see in this right panel.
# Scroll down to begin reading the code behind the stagerunner package.

##   * import data
##   * clean data
##      * __impute variable 1__
##      * __discretize variable 2__
##   * __train model__
## 
## The return value will be `list(F, list(T, T), T)`. If instead we
## want the stages *before* "impute variable 1", the return value
## will be `list(T, list(T, F), F)`.
##
##   * __import data__
##   * __clean data__
##      * __impute variable 1__
##      * discretize variable 2
##   * train model
##
#' Fill a nested logical list with TRUEs before or after the first TRUE
#' 
#' This is a helper function to implement the \code{to} parameter
#' in the \code{run} method on a stageRunner object.
#'
#' @seealso \code{\link{stageRunner__run}}
#' @name boolean_fill
#' @param el list. A nested list of logicals with exactly one entry \code{TRUE}.
#' @param forward logical. \code{FALSE} for backwards, and \code{TRUE} for forwards.
#'   The default is \code{TRUE}.
#' @return the filled list
boolean_fill <- function(el, forward = TRUE) {
  ## We now dig into the actual code. The `!is.finite` condition on the right
  ## works due to the behavior of `which`. If no element of the (possibly nested)
  ## list `el` contains a `TRUE`, it will return a zero-length vector. When we 
  ## subset to `[1]`, we get `NA`, which fails `is.finite`.
  ix <- which(vapply(el, contains_true, logical(1)))[1]
  if (!is.finite(ix)) stop("boolean_fill called but no TRUEs found")

  ## We could have checked that there is a `TRUE` somewhere in `el` more
  ## elegantly, but we need the precise location, `ix`.
  if (isTRUE(forward)) {
    fills <- seq_len(length(el) - ix) + ix
  } else {
    fills <- seq_len(ix - 1)
  }
  el[fills] <- TRUE
  
  ## If the sequence of slots before or after the index which contains a `TRUE`
  ## (according to the value of `forward`) have been flattened to a `TRUE` value,
  ## the only remaining `TRUE` flattening has to occur recursively in any
  ## remaining list elements.
  if (!is.atomic(el[[ix]])) {
    el[[ix]] <- boolean_fill(el[[ix]], forward = forward)
  }
  el
}

