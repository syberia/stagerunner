#' Generic for printing stageRunner objects.
#' 
#' @name stageRunner_show
#' @param indent integer. Internal parameter for keeping track of nested
#'   indentation level.
stageRunner_show <- function(indent = 0) {
  if (missing(indent)) {
    sum_stages <- function(x) sum(vapply(x,
      function(x) if (is.stagerunner(x)) sum_stages(x$stages) else 1L, integer(1)))
    caching <- if (self$remember) ' caching' else ''
    cat("A", caching, " stageRunner with ", sum_stages(self$stages), " stages:\n", sep = '')
  }
  stage_names <- names(self$stages) %||% rep("", length(self$stages))

  # A helper function for determining if a stage has been run yet.
  began_stage <- function(stage)
    if (is.stagerunner(stage)) any(vapply(stage$stages, began_stage, logical(1)))
    else if (is.stageRunnerNode(stage)) !is.null(stage$.cached_env)
    else FALSE

  lapply(seq_along(stage_names), function(index) {
    prefix <- paste0(rep('  ', (if (is.numeric(indent)) indent else 0) + 1), collapse = '')
    marker <-
      if (self$remember && began_stage(self$stages[[index]])) {
        next_stage <- treeSkeleton$new(self$stages[[index]])$last_leaf()$successor()$object
        if (( is.null(next_stage) && !self$.root()$.finished) ||
            (!is.null(next_stage) && !began_stage(next_stage))) 
          '*' # Use a * if this is the next stage to be executed
          # TODO: Fix the bug where we are unable to tell if the last stage
          # finished without a .finished internal field.
          # We need to look at and set predecessors, not successors.
        else '+' # Other use a + for completely executed stage
      } else '-'
    prefix <- gsub('.$', marker, prefix)
    stage_name <- 
      if (is.na(stage_names[[index]]) || stage_names[[index]] == "")
        paste0("< Unnamed (stage ", index, ") >")
      else stage_names[[index]]
    cat(prefix, stage_name, "\n")
    if (is.stagerunner(self$stages[[index]]))
      self$stages[[index]]$show(indent = indent + 1)
  })

  if (missing(indent)) { cat('Context '); print(self$.context) }
  NULL
}

