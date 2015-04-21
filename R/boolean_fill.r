## A stagerunner describes a *linear* sequence of execution: import data,
## perform this munging step, then that munging, then do some modeling, etc.
## However, it is structured hierarchically as a nested list for easier
## usability. This function will created a nested list with the exact same
## structure as the stagerunner except that each terminal node is either
## TRUE or FALSE. 
##
## Specifically, given a tree structure with exactly one TRUE value in the
## terminal nodes, all successors of that node will be marked as TRUE
## as well. Conversely, if `forward = FALSE`, then all predecessors of
## that node will be marked as TRUE.
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
  ix <- which(vapply(el, contains_true, logical(1)))[1]
  ## If no value is TRUE, `which` returns a zero-length vector, and subsetting
  ## by `[1]` gives `NA`.
  if (!is.finite(ix)) stop("boolean_fill called but no TRUEs found")

  if (isTRUE(forward)) {
    fills <- seq_len(length(el) - ix) + ix
  } else {
    fills <- seq_len(ix - 1)
  }
  el[fills] <- TRUE
  
  ## If the sequence of slots before or after the index which contains a TRUE
  ## (according to the value of `forward`) have been flattened to a TRUE value,
  ## the only remaining TRUE flattening has to occur recursively in any
  ## remaining list elements.
  if (!is.atomic(el[[ix]])) {
    el[[ix]] <- boolean_fill(el[[ix]], forward = forward)
  }
  el
}

