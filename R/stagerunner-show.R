## Printing a stagerunner should show information about:
##
##    1. The hierarchical structure of its stages.
##    2. A summary of what stages have been already executed.
##
## We choose the following notation:
##
## ```
## A caching stageRunner with 4 stages:
## + import
##  * data
##    + impute variable
##   * discretize variable
##  - train model
## Context <environment: 0x101726640>
## ```
## 
## The `+` indicates the stage has been executed successfully; `*` indicates
## it is currently being executed; and `-` means the stage has not yet
## been executed. If `remember = FALSE`, this information is not available,
## so we only use this prefix notation for "caching stagerunners" (those
## with `remember = TRUE`).
##
#' Generic for printing stageRunner objects.
#' 
#' @name stageRunner_show
#' @param indent integer. Internal parameter for keeping track of nested
#'   indentation level.
stageRunner_show <- function(indent = 0) {
  if (missing(indent)) {
    sum_stages <- function(x) sum(vapply(x,
      function(x) if (is.stagerunner(x)) sum_stages(x$stages) else 1L, integer(1)))
    caching <- if (self$remember) " caching"
    cat("A", caching, " stageRunner with ",
        sum_stages(self$stages), " stages:\n", sep = '')
  }

  stage_names <- names(self$stages) %||% rep("", length(self$stages))

  for (index in seq_along(stage_names)) {
    prefix <- paste0(rep('  ', (if (is.numeric(indent)) indent else 0) + 1), collapse = '')
    currently_executing_this_stage <- self$remember && began_stage(self$stages[[index]])

    if (currently_executing_this_stage) {
      next_stage <- treeSkeleton$new(self$stages[[index]])$last_leaf()$successor()$object
      if (( is.null(next_stage) && !self$.root()$.finished) ||
          (!is.null(next_stage) && !began_stage(next_stage)))
        marker <- '*' # Use a * if this is the next stage to be executed
        # TODO: Fix the bug where we are unable to tell if the last stage
        # finished without a .finished internal field.
        # We need to look at and set predecessors, not successors.
      else {
       marker <- '+' # Other use a + for completely executed stage
      }
    } else {
      marker <- '-'
    }

    prefix <- gsub('.$', marker, prefix)
    if (is.na(stage_names[[index]]) || stage_names[[index]] == "") {
      stage_name <- paste0("< Unnamed (stage ", index, ") >")
    } else {
      stage_name <- stage_names[[index]]
    }

    cat(prefix, stage_name, "\n")

    if (is.stagerunner(self$stages[[index]])) {
      self$stages[[index]]$show(indent = indent + 1)
    }
  }

  if (missing(indent)) {
    cat('Context ')
    print(self$.context)
  }

  NULL
}

# A helper function for determining if a stage has been run yet.
began_stage <- function(stage) {
  if (is.stagerunner(stage)) {
    any(vapply(stage$stages, began_stage, logical(1)))
  } else if (is.stageRunnerNode(stage)) {
    node <- treeSkeleton(stage)$predecessor()$object
    is.null(node) || node$executed
  }
}

#' @export
print.stageRunner <- function(x, ...) {
  x$show(...)
}

#' @export
print.stageRunnerNode <- function(x, ...) {
  x$show(...)
}

