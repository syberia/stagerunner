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
  object <<- object

  # Make sure parent_caller and children_caller are methods of object
  if (is.refClass(object)) {
    stopifnot(all(c(parent_caller, children_caller) %in% object$getRefClass()$methods()))
  } else if (inherits(object, "R6")) {
    stopifnot(all(c(parent_caller, children_caller) %in% ls(sr)))
  }

  parent_caller <<- parent_caller
  children_caller <<- children_caller
  NULL
}

#' Attempt to find the successor of the current node.
#'
#' @name treeSkeleton__successor
#' @param index integer. If specified, this is the index of the current node
#'   in the children of its parent. (Sometimes, this cannot be computed
#'   automatically, and should usually be provided.)
#' @return successor for the wrapped object.
treeSkeleton__successor <- function(index = NULL) {
  if (is.null(p <- parent())) return(NULL) # no successor of root node

  parent_index <- if (is.null(index)) .parent_index() else index
  stopifnot(is.finite(parent_index))

  # If we are the last leaf in the list of our parent's children,
  # our successor is our parent's successor
  if (parent_index == length(p$children()))
    p$successor()
  else
    p$children()[[parent_index + 1]]$first_leaf()
}


#' Find the root node of the tree (the only one with no parent).
#'
#' @name treeSkeleton__root
#' @return The root node of the tree or NULL if empty tree.
treeSkeleton__root <- function() {
  if (is.null(.self$parent())) .self
  else .self$parent()
}

#' Find the first leaf in a tree.
#'
#' @name treeSkeleton__first_leaf
#' @return The first leaf, that is, the first terminal child node.
treeSkeleton__first_leaf <- function() {
  if (length(children()) == 0) .self
  else children()[[1]]$first_leaf()
}

#' Find the last leaf in a tree.
#'
#' @name treeSkeleton__last_leaf
#' @return The last leaf, that is, the last terminal child node.
treeSkeleton__last_leaf <- function() {
  if (length(childs <- .self$children()) == 0) .self
  else childs[[length(childs)]]$last_leaf()
}

#' Find the parent of the current object wrapped in a treeSkeleton.
#' @name treeSkeleton__parent
treeSkeleton__parent <- function() {
  if (!inherits(.parent, 'uninitializedField')) return(.parent)
  .parent <<-
    if (is.null(obj <- OOP_type_independent_method(object, parent_caller))) NULL
    else treeSkeleton$new(obj, parent_caller = parent_caller,
                          children_caller = children_caller)
}

#' Find the children of the current object wrapped in treeSkeletons.
#' @name treeSkeleton__children
treeSkeleton__children <- function() {
  if (!inherits(.children, 'uninitializedField')) return(.children)
  prechildren <- OOP_type_independent_method(object, children_caller)
  .children <<- lapply(prechildren, treeSkeleton$new,
                       parent_caller = parent_caller)
}

#' Find the index of the current object in the children of its parent.
#' @name treeSkeleton__.parent_index
treeSkeleton__.parent_index <- function() {
  if (!is.null(ci <- attr(object, 'child_index'))) ci
  # Hack for accessing attribute modifications on a reference class object
  # See: http://stackoverflow.com/questions/22752021/why-is-r-capricious-in-its-use-of-attributes-on-reference-class-objects
  else if (is.refClass(object) &&
           !is.null(ci <- attr(attr(object, '.xData')$.self, 'child_index'))) ci
  else # look through the parent's children and compare to .self
    # Danger Will Robinson! This will lead to strange bugs if our tree
    # has several nodes with duplicate objects
    which(vapply(
      .self$parent()$children(),
      function(node) identical(node$object, object), logical(1)))[1]
}

#' Find the key with the given index using the names of the lists
#' that parametrize each node's children.
#'
#' For example, if our tree structure is given by
#'   \code{list(a = list(b = 1, c = 2))}
#' then calling \code{find('a/b')} on the root node will return \code{1}.
#'
#' @name treeSkeleton__find
#' @param key character. The key to find in the given tree structure,
#'    whether nodes are named by their name in the \code{children()}
#'    list. Numeric indices can be used to refer to unnamed nodes.
#'    For example, if key is \code{a/2/b}, this method would try to find
#'    the current node's child \code{a}'s second child's \code{b} child.
#'    (Just look at the examples).
#' @return the subtree or terminal node with the given key.
#' @examples 
#' \dontrun{
#' sr <- stageRunner$new(new.env(), list(a = list(force, list(b = function(x) x + 1))))
#' stagerunner:::treeSkeleton$new(sr)$find('a/2/b') # function(x) x + 1
#' }
treeSkeleton__find <- function(key) {
  stopifnot(is.character(key))
  if (length(key) == 0 || identical(key, '')) return(object)
  # Extract "foo" from "foo/bar/baz"
  subkey <- regmatches(key, regexec('^[^/]+', key))[[1]]
  key_remainder <- substr(key, nchar(subkey) + 2, nchar(key))
  if (grepl('^[0-9]+', subkey)) {
    subkey <- as.integer(subkey)
    key_falls_within_children <- length(children()) >= subkey
    stopifnot(key_falls_within_children)
  } else {
    matches <- grepl(subkey, names(children()))
    stopifnot(length(matches) == 1)
    key <- which(matches)
  }
  children()[[key]]$find(key_remainder)
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
#'
#' @docType class
#' @name treeSkeleton
treeSkeleton <- setRefClass('treeSkeleton',
  fields = list(object = 'ANY', parent_caller = 'character',
                children_caller = 'character', .children = 'ANY',
                .parent = 'ANY'),
  methods = list(
    initialize    = stagerunner:::treeSkeleton__initialize,
    successor     = stagerunner:::treeSkeleton__successor,
    # TODO: I don't need any more iterators, but maybe implement them later
    #predecessor  = stagerunner:::treeSkeleton__predecessor,
    parent        = stagerunner:::treeSkeleton__parent,
    children      = stagerunner:::treeSkeleton__children,
    root          = stagerunner:::treeSkeleton__root,
    first_leaf    = stagerunner:::treeSkeleton__first_leaf,
    last_leaf     = stagerunner:::treeSkeleton__last_leaf,
    find          = stagerunner:::treeSkeleton__find,
    .parent_index = stagerunner:::treeSkeleton__.parent_index,
    show          = function() { cat("treeSkeleton wrapping:\n"); print(object) }
  )
)

is.refClass <- function(obj) {
  inherits(obj, "refClass") && !inherits(obj, "R6")
}

