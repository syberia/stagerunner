## Stagerunners are [tree structures](https://en.wikipedia.org/wiki/Tree_(data_structure))
## and come with a natural set of operations, like taking the predecessor,
## successor, and root of a node. However, these are not entirely simple to
## implement in a manner that is implementation-independent.
##
## Specifically, we recognize that the notion of a node successor and
## predecessor is implementation agnostic as long as we have access
## to class methods that provide access to a node's parent and children.
## In this case, we can write an implementation-agnostic version that
## works regardless of whether the object is an S3, S4, or R6
## object.
#' @include treeSkeleton-initialize.R treeSkeleton-predecessor.R
#'    treeSkeleton-successor.R treeSkeleton-parent_index.R
NULL

#' Find the root node of the tree (the only one with no parent).
#'
#' @name treeSkeleton__root
#' @return The root node of the tree or list() if empty tree.
treeSkeleton__root <- function() {
  if (length(self$parent()) == 0) self
  else self$parent()
}

#' Find the first leaf in a tree.
#'
#' @name treeSkeleton__first_leaf
#' @return The first leaf, that is, the first terminal child node.
treeSkeleton__first_leaf <- function() {
  if (length(self$children()) == 0) self
  else self$children()[[1]]$first_leaf()
}

#' Find the last leaf in a tree.
#'
#' @name treeSkeleton__last_leaf
#' @return The last leaf, that is, the last terminal child node.
treeSkeleton__last_leaf <- function() {
  if (length(childs <- self$children()) == 0) self
  else childs[[length(childs)]]$last_leaf()
}

#' Find the parent of the current object wrapped in a treeSkeleton.
#' @name treeSkeleton__parent
treeSkeleton__parent <- function() {
  if (!is.unitialized_field(self$.parent)) return(self$.parent)
  self$.parent <-
    if (is.null(obj <- OOP_type_independent_method(self$object, self$parent_caller))) NULL
    else treeSkeleton$new(obj, parent_caller = self$parent_caller,
                          children_caller = self$children_caller)
}

#' Find the children of the current object wrapped in treeSkeletons.
#' @name treeSkeleton__children
treeSkeleton__children <- function() {
  if (!is.unitialized_field(self$.children)) return(self$.children)
  prechildren <- OOP_type_independent_method(self$object, self$children_caller)
  self$.children <- lapply(prechildren, treeSkeleton$new,
                       parent_caller = self$parent_caller)
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
## Currently out of service! Will be back shortly.
#  stopifnot(is.character(key))
#  if (length(key) == 0 || identical(key, '')) return(self$object)
#  # Extract "foo" from "foo/bar/baz"
#  subkey <- regmatches(key, regexec('^[^/]+', key))[[1]]
#  key_remainder <- substr(key, nchar(subkey) + 2, nchar(key))
#  if (grepl('^[0-9]+', subkey)) {
#    subkey <- as.integer(subkey)
#    key_falls_within_children <- length(self$children()) >= subkey
#    stopifnot(key_falls_within_children)
#  } else {
#    matches <- grepl(subkey, names(self$children()))
#    stopifnot(length(matches) == 1)
#    key <- which(matches)
#  }
#  self$children()[[key]]$find(key_remainder)
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
#' @name treeSkeleton
#' @docType class
#' @format NULL
treeSkeleton_ <- R6::R6Class('treeSkeleton',
  public = list(
    object = 'ANY',
    ## As long as we know how to get an objects parent and children,
    ## we will be able to determine all the nice derived methods below.
    parent_caller = 'character',
    children_caller = 'character',
    .children = 'ANY',
    .parent = 'ANY',

    initialize    = treeSkeleton__initialize,
    successor     = treeSkeleton__successor,
    predecessor   = treeSkeleton__predecessor,
    parent        = treeSkeleton__parent,
    children      = treeSkeleton__children,
    root          = treeSkeleton__root,
    first_leaf    = treeSkeleton__first_leaf,
    last_leaf     = treeSkeleton__last_leaf,
    find          = treeSkeleton__find,
    .parent_index = treeSkeleton__.parent_index,
    show          = function() { cat("treeSkeleton wrapping:\n"); print(self$object) }
  )
)

## Some fancy tricks to make `treeSkeleton(...)` and `treeSkeleton(...)`
## have the same effect, just like in traditional reference classes.
#' @export
treeSkeleton <- structure(
  function(...) { treeSkeleton_$new(...) },
  class = "treeSkeleton_"
)

#' @export
`$.treeSkeleton_` <- function(...) {
  stopifnot(identical(..2, "new"))
  ..1
}

uninitialized_field <- function() {
  structure(list(), class = "uninitialized_field")
}

is.unitialized_field <- function(x) {
  is(x, "uninitialized_field")
}

