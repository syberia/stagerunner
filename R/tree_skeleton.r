#' Initialize a treeSkeleton object.
#'
#' treeSkeleton objects allow you to traverse a reference class object
#' as if it had a tree structure, merely by knowing how to call parent
#' or child nodes.
#'
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
  object <<- object

  # Make sure parent_caller and children_caller are methods of object
  if (inherits(object, 'refClass'))
    stopifnot(all(c(parent_caller, children_caller) %in% object$getRefClass()$methods()))

  parent_caller <<- parent_caller
  children_caller <<- children_caller
}


#' Attempt to find the successor of the current node.
#'
#' @param index integer. If specified, this is the index of the current node
#'   in the children of its parent. (Sometimes, this cannot be computed
#'   automatically, and should usually be provided.)
#' @return successor for the wrapped object.
treeSkeleton__successor <- function(index = NULL) {
  parent_index <- if (is.null(index)) .parent_index() else index
  stopifnot(is.finite(parent_index))
  parent_index
}

#' Find the parent of the current object wrapped in a treeSkeleton.
treeSkeleton__parent <- function() {
  treeSkeleton$new(
    if (inherits(object, 'refClass'))
      # bquote and other methods don't work here -- it's hard to dynamically
      # fetch reference class methods!
      eval(parse(text = paste0('`$`(object, "', parent_caller, '")()')))
    else if (parent_caller %in% names(attributes(object)))
      attr(object, parent_caller)
    else get(parent_caller)(object)
  , parent_caller = parent_caller, children_caller = children_caller)
}

#' Find the children of the current object wrapped in treeSkeletons.
treeSkeleton__children <- function() {
  lapply(
    if (inherits(object, 'refClass'))
      # bquote and other methods don't work here -- it's hard to dynamically
      # fetch reference class methods!
      eval(parse(text = paste0('`$`(object, "', children_caller, '")()')))
    else if (children_caller  %in% names(attributes(object)))
      attr(object, children_caller)
    else get(children_caller)(object)
  , function(child) treeSkeleton$new(child, parent_caller = parent_caller,
                                     children_caller = children_caller)
  )
}

#' Find the index of the current object in the children of its parent.
treeSkeleton__.parent_index <- function() {
  which(vapply(
    object$parent()$children(),
    function(node) identical(node, object), logical(1)))[1]
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
  fields = list(object = 'ANY', parent_caller = 'character',
                children_caller = 'character'),
  methods = list(
    initialize    = stagerunner:::treeSkeleton__initialize,
    successor     = stagerunner:::treeSkeleton__successor,
    parent        = stagerunner:::treeSkeleton__parent,
    children      = stagerunner:::treeSkeleton__children,
    #predecessor  = stagerunner:::treeSkeleton__predecessor,
    .parent_index = stagerunner:::treeSkeleton__.parent_index 
  )
)

