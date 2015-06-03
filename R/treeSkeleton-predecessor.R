#' Attempt to find the predecessor of the current node.
#'
#' @name treeSkeleton__predecessor
#' @param index integer. If specified, this is the index of the current node
#'   in the children of its parent. (Sometimes, this cannot be computed
#'   automatically, and should usually be provided.)
#' @return predecessor for the wrapped object.
treeSkeleton__predecessor<- function(index = NULL) {
  if (is.null(p <- self$parent())) return(NULL) # no predecessor of root node

  parent_index <- if (is.null(index)) self$.parent_index() else index
  stopifnot(is.finite(parent_index))

  # If we are the first leaf in the list of our parent's children,
  # our predecessor is our parent's successor
  if (parent_index == 1) {
    p$predecessor()
  } else {
    p$children()[[parent_index - 1]]$last_leaf()
  }
}

