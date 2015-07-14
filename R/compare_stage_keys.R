## A stagerunner is simply a linear sequence (of usually functions)
## that is packaged as a tree structure to make it easier to reference
## related groups of operations.
##
## Since stagerunners are intended to be run sequentially, that is,
## only backward to forwards rather than the other way around, it is
## important to be able to identify when it is accidentally run
## in the latter way. For example, if we have three stages and
## a runner is called in the wrong order with `runner$run(2, 1)`,
## we expect stage 1 to execute before stage 2.
##
## The point of `compare_stage_keys` is to determine whether a later
## stage has been called prior to an earlier stage. If that is
## the case, this function will return `TRUE`, and later in the internals
## of the stagerunner object we will be able to flip the keys.
#' Compare two stage keys to see which one has a stage run first
#' 
#' This is a helper function to implement the \code{to} parameter
#' in the \code{run} method on a stageRunner object.
#'
#' @seealso \code{\link{run}}
#' @name compare_stage_keys
#' @param key1 list
#' @param key2 list
#' @return logical. Whether or not key1 runs a stage before key2.
compare_stage_keys <- function(key1, key2) {
  if (length(key1) == 0 || isTRUE(key1)) return(TRUE)
  index_of_true <- function(el) which(vapply(el, contains_true, logical(1)))[1]
  ## `ix` will be a vector of the location in the list structure of `key1` and
  ## `key2` where the first (and, if they are correctly formed, the only)
  ## `TRUE` occurs.
  ix <- vapply(list(key1, key2), index_of_true, numeric(1))

  ## If no element in the first structure has a `TRUE`, the first key definitely
  ## cannot run a stage first (since no stages have been marked for running!).
  if (!is.finite(ix[1])) {
    return(FALSE)
  ## Otherwise, if the latter has no `TRUE`s, the first key definitely runs first.
  } else if (!is.finite(ix[2])) { return(TRUE) }

  if (ix[1] == ix[2]) {
    ## If the keys specify the exact same stage in the stagerunner, there is
    ## a tie and we may as well return TRUE.
    if (is.atomic(key1) && is.atomic(key2)) {
      stop("WTF")
      TRUE
    } else {
    ## Otherwise, one of the two keys must be another list, so we can 
    ## recursively determine which key runs first.
      compare_stage_keys(key1[[ix[1]]], key2[[ix[2]]])
    }
    ## We now come across a special case. Imagine we have
    ## a stagerunner as before.
    ##
    ##   * import data
    ##   * clean data
    ##      * impute variable 1
    ##      * __remove outliers from variable 1__
    ##      * __discretize variable 2__
    ##   * train model
    ## 
    ## If we call `runner$run("clean data/2", "clean data")` this will signify
    ## to "run from 'remove outliers from variable 1' to the end of the
    ## 'clean data' stage" (i.e., until "discretize variable 2").
    ## 
    ## However, note in this case that the two keys would be represented by
    ## `list(F, list(F, T, T), F)` and `list(F, T, F)`. If we are not
    ## careful, the `compare_stage_keys` function will indicate that
    ## the latter occurs before the former, the keys will be flipped,
    ## and we will end up executing "impute variable 1" instead!
    ##
    ##   * import data
    ##   * clean data
    ##      * __impute variable 1__ (the unintended effect)
    ##      * __remove outliers from variable 1__
    ##      * discretize variable 2
    ##   * train model

    ## We solve this problem by returning `TRUE` if `key2` consists purely
    ## of `TRUE`s (i.e., if it signifies "run until the end of this stage").
  } else if (all(vapply(key2, isTRUE, logical(1)))) {
    TRUE 
  } else { 
    ## Finally, if the keys are directly comparable, the first key should be run
    ## earlier if and only if the first key contains a `TRUE` earlier than the
    ## second key.
    ix[1] < ix[2]
  }
}


