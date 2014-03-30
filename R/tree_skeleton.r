#' Initialize a treeSkeleton object.
#'
#' treeSkeleton objects allow you to traverse a reference class object
#' as if it had a tree structure, merely by knowing how to call parent
#' or child nodes.
#'
#' @param object refClass. Any reference class object.
#' @param parent_caller character. The name of the reference class method
#'    that returns the parent object, if the object was a node in a tree
#'    structure.
#' @param children_caller character. The name of the reference class method
#'    that returns the child objects, if the object was a node in a tree
#'    structure.
#' @return a treeSkeleton object.
treeSkeleton__initialize <- function(object, parent_caller, child_caller) {
  object <<- object
  # Make sure parent_caller and child_caller are methods of object
  stopifnot(all(c(parent_caller, child_caller) %in% object$getRefClass()$methods()))
  parent_caller <<- parent_caller
  children_caller <<- children_caller
}

#' This class implements iterators for a tree-based structure
#' without an actual underlying tree.
#'
#' In other dynamic languages, this kind of behavior would be called
#' duck typing. Imagine we have an object \code{x} that is of some
#' reference class. This object has a tree structure, and each node
#' in the tree has a parent and children. However, the methods to
#' fetch a node's parent or its children may have arbitrary names.
#' These names are stored in \code{treeSkeleton}'s \code{parent_caller}
#' and \code{children_caller} fields. Thus, if \code{x$methods()}
#' refers to \code{x}'s children and \code{x$parent_method()} refers
#' to \code{x}'s parent, we could define a \code{treeSkeleton} for
#' \code{x} by writing \code{treeSkeleton$new(x, 'parent_method', 'methods')}.
#'
#' The iterators on a \code{treeSkeleton} use the standard definition of
#' successor, predecessor, ancestor, etc.
#' @docType refClass
#' @name treeSkeleton
treeSkeleton <- setRefClass('treeSkeleton',
  fields = list(object = 'refClass', parent_caller = 'character',
                children_caller = 'character'),
  methods = list(
    initialize   = stagerunner:::treeSkeleton__initialize#,
    #successor    = stagerunner:::treeSkeleton__successor,
    #predecessor  = stagerunner:::treeSkeleton__successor
  )
)

