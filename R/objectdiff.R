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
#' @param prefix character. The prefix to assign to this stageRunner.
#' @name stageRunner__.set_prefixes
stageRunner__.set_prefixes <- function(prefix = '') {
  .prefix <<- prefix
  for (i in seq_along(stages)) {
    if (is.stageRunner(stages[[i]])) {
      stages[[i]]$.set_prefixes(paste0(prefix, i, '/'))
    }
  }
}

`first_commit?` <- function(commit) {
  all(strsplit(commit, "/", fixed = TRUE)[[1]] == '1')
}

stageRunner_.set_prefixes <- function(prefix = '') {
  .prefix <<- prefix
  for (i in seq_along(stages)) {
    if (is.stageRunner(stages[[i]])) {
      stages[[i]]$.set_prefixes(paste0(prefix, i, '/'))
    }
  }
}

