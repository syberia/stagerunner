## A stagerunner is simply a linear sequence (of usually functions)
## that is packaged as a tree structure to make it easier to reference
## related groups of operations.
##
## Since stagerunners are intended to be run sequentially, that is,
## only backward to forwards rather than the other way around, it is
## important to be able to identify when it is accidentally run
## in the latter way. For example, if we have three stages and
## a runner is called with `$run(2, 1)`, we expect it to be run
## in the reverse order with stages 1 and 2 executed first.
##
## The point of `compare_stage_keys` is to determine whether a later
## stage has been called prior to an earlier stage. If that is
## the case, this function will return TRUE.
#' Compare two stage keys to see which one has a stage run first
#' 
#' This is a helper function to implement the \code{to} parameter
#' in the \code{run} method on a stageRunner object.
#'
#' @seealso \code{\link{stageRunner__run}}
#' @name compare_stage_keys
#' @param key1 list
#' @param key2 list
#' @return logical. Whether or not key1 runs a stage before key2.
compare_stage_keys <- function(key1, key2) {
  if (length(key1) == 0 || isTRUE(key1)) return(TRUE)
  index_of_true <- function(el) which(vapply(el, contains_true, logical(1)))[1]
  ## `ix` will be a vector of where in the list structure of `key1` and `key2`
  ## the first (and, if they are correctly formed, the only) TRUE occurs.
  ix <- vapply(list(key1, key2), index_of_true, numeric(1))

  ## If no element in the first structure has a TRUE, the first key definitely
  ## cannot run a stage first (since no stages have been marked for running!).
  if (!is.finite(ix[1])) {
    return(FALSE)
  ## Otherwise, if the latter has no TRUEs, the first key definitely runs first.
  } else if (!is.finite(ix[2])) { return(TRUE) }

  if (ix[1] == ix[2]) {
    ## If the keys specify the exact same stage in the stagerunner, there is
    ## a tie and we may as well return TRUE.
    if (is.atomic(key1) && is.atomic(key2)) {
      TRUE
    ## Otherwise, one of the two keys must be another list, so we can 
    ## recursively determine which key runs first.
    } else {
      compare_stage_keys(key1[[ix[1]]], key2[[ix[2]]])
    }
  ## We now come across a special careful special case. Imagine we have
  ## a stagerunner with stages "import", "munge/impute" and
  ## "munge/replace vals". If we call `runner$run("munge/replace", "munge")`
  ## this should mean to run "from 'munge/replace vals' until the end of
  ## the enclosing munge stage." See the test case for
  ## "it correctly goes to the end of a section" in `test-stage_runner.r`.
  ## If you think about it hard enough, you should be able to convince
  ## yourself that this logic works even for nested stages.
  } else if (all(vapply(key2, isTRUE, logical(1)))) {
    TRUE 
  ## If the keys are directly comparable, the first key should be run first
  ## if and only if the first key contains a TRUE earlier than the second key.
  } else { 
    ix[1] < ix[2]
  }
}


