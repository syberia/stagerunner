# Some features of stageRunner, specifically its interaction with the
# objectdiff package, require some additional setup. We attempt to record
# all of these dependencies in this file.

#' Set all prefixes for child stageRunners.
#'
#' When a stageRunner is used in conjunction with an
#' \code{objectdiff::tracked_environment}, we need to remember
#' the full nested tree structure. This function sets up the
#' \code{prefix} member of each sub-stageRunner recursively to enable
#' correct remembering functionality.
#'
#' @name stageRunner__.set_prefixes
stageRunner__.set_prefixes <- function() {
}
