#' Find the index of the current object in the children of its parent.
#' @name treeSkeleton__.parent_index
treeSkeleton__.parent_index <- function() {
  if (!is.null(ci <- attr(self$object, 'child_index'))) ci
  # Hack for accessing attribute modifications on a reference class object
  # See: http://stackoverflow.com/questions/22752021/why-is-r-capricious-in-its-use-of-attributes-on-reference-class-objects
  else if (inherits(self$object, 'refClass') && !inherits(self$object, 'R6') &&
           !is.null(ci <- attr(attr(self$object, '.xData')$.self, 'child_index'))) ci
  else # look through the parent's children and compare to .self
    # Danger Will Robinson! This will lead to strange bugs if our tree
    # has several nodes with duplicate objects
    which(vapply(
      self$parent()$children(),
      function(node) identical(node$object, self$object), logical(1)))[1]
}

