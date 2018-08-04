## Stagerunners remember the *full history* of their execution. If you have
## fifty data preparation steps recorded in a stagerunner and the `remember`
## flag is set to `TRUE`, a full copy of the dataset will be made after
## each step. This is highly inefficient.
##
## We attempt to solve this problem with a space-time tradeoff: the 
## [objectdiff](http://github.com/syberia/objectdiff) package computes
## the *difference* between the environment before and after executing
## a given stage. By incorporating this package into a stagerunner,
## we can take slightly more time (by computing differences between
## environments during the execution of each stage) but save a lot
## of memory (by only storing *patches* that record what has changed
## during each step, rather than a full copy of the data set).
##
## This advanced feature allows stagerunners to remain in-memory,
## retaining the fast interactive iterate model building process.
## The downside is either slightly more space or time usage depending
## on the configuration of the objectdiff package.
##
## We can avoid the problem entirely by doing all of our processing
## in batches or in-database, but this is outside of the scope of
## this package. For interactive model development on data sets
## with less than 1M rows, performance is usually not prohibitive.
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
#' @name stageRunner_.set_prefixes
stageRunner_.set_prefixes <- function(prefix = '') {
  self$.prefix <- prefix
  for (i in seq_along(self$stages)) {
    if (is.stageRunner(self$stages[[i]])) {
      self$stages[[i]]$.set_prefixes(paste0(prefix, i, '/'))
    }
  }
}

`first_commit?` <- function(commit) {
  all(strsplit(commit, "/", fixed = TRUE)[[1]] == '1')
}


