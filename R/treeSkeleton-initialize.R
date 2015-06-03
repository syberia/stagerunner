#' Initialize a treeSkeleton object.
#'
#' treeSkeleton objects allow you to traverse a reference class object
#' as if it had a tree structure, merely by knowing how to call parent
#' or child nodes.
#'
#' @name treeSkeleton__initialize
#' @param object ANY. If a reference class object, then \code{parent_caller}
#'    and \code{children_caller} will refer to reference class methods.
#'    If an attribute on the object with names of \code{children_caller} and
#'    \code{parent_caller} exists, those will be used. Otherwise, the
#'    generic methods will be used.
#' @param parent_caller character. The name of the reference class method
#'    that returns the parent object, if the object was a node in a tree
#'    structure.
#' @param children_caller character. The name of the reference class method
#'    that returns the child objects, if the object was a node in a tree
#'    structure.
#' @return a treeSkeleton object.
treeSkeleton__initialize <- function(object, parent_caller = 'parent',
                                     children_caller = 'children') {
  stopifnot(!is.null(object))
  self$object  <- object
  self$.parent   <- uninitialized_field()
  self$.children <- uninitialized_field()

  # Make sure parent_caller and children_caller are methods of object
  if (inherits(object, "R6")) {
    stopifnot(all(c(parent_caller, children_caller) %in% ls(object)))
  }

  self$parent_caller   <- parent_caller
  self$children_caller <- children_caller
  NULL
}
