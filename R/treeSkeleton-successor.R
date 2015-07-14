#' Attempt to find the successor of the current node.
#'
#' @name treeSkeleton__successor
#' @param index integer. If specified, this is the index of the current node
#'   in the children of its parent. (Sometimes, this cannot be computed
#'   automatically, and should usually be provided.)
#' @return successor for the wrapped object.
treeSkeleton__successor <- function(index = NULL) {
  ## We define the successor of the root node as `NULL`.
  if (is.null(p <- self$parent())) return(NULL) # no successor of root node

  parent_index <- if (is.null(index)) self$.parent_index() else index
  stopifnot(is.finite(parent_index))

  ## If we are the last leaf in the list of our parent's children,
  ## our successor is our parent's successor
  if (parent_index == length(p$children())) {
    p$successor()
  } else {
    ## Otherwise, the successor is the first leaf of the next child node.
    p$children()[[parent_index + 1]]$first_leaf()
  }
}

