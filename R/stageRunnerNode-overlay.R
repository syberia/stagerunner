#' Append one stageRunnerNode around another.
#'
#' @param other_node stagerunner or stageRunnerNode.
#' @param label character. Under the hood, this will be the "stage name"
#'    for the stage represented by the \code{other_node} in the
#'    automatically generated new stageRunner used as this node's
#'    callable (assuming \code{flat} is \code{FALSE}).
#' @param flat logical. If \code{TRUE}.
#' @return \code{TRUE} or \code{FALSE} according as the wrapping was
#'    successful.
#' @examples \dontrun{
#' node1 <- stageRunnerNode(function(e) print(2))
#' node2 <- stageRunnerNode(function(e) { print(1); yield(); print(3); })
#' node1$around(node2)
#' node1$run() # Will print 1 2 3
#' # Notice the provided "yield" keyword, which allows calling the
#' # node that is being wrapped.
#' }
stageRunnerNode_overlay <- function(other_node, label = NULL, flat = FALSE) {
  if (is.stageRunnerNode(other_node)) other_node <- other_node$callable
  if (is.null(other_node)) return(FALSE)
  if (!is.stagerunner(other_node)) 
    other_node <- stageRunner$new(self$.context, other_node)

  # Coerce the current callable object to a stageRunner so that
  # we can append the other_node's stageRunner.
  if (!is.stagerunner(self$callable)) 
    self$callable <- stageRunner$new(self$.context, self$callable)

  # TODO: Fancier merging here
  if (isTRUE(flat)) {
    if (!is.character(label)) stop("flat coalescing needs a label")
    self$callable$stages[[label]] <- other_node
  } else self$callable$append(other_node, label)
}
