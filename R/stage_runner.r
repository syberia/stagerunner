#' Initialize a stageRunner object.
#'
#' stageRunner objects are used for executing a linear sequence of
#' actions on a context (an environment). For example, if we have an
#' environment \code{e} containing \code{x = 1, y = 2}, then if
#' \code{stages = list(function(e) e$x <- e$x + 1, function(e) e$y <- e$y - e$x)}
#' will cause \code{x = 2, y = 0} after running the stages.
#'
#' @param context an environment. The initial environment that is getting
#'    modified during the execution of the stages. 
#' @param stages a list. The functions to execute on the \code{context}.
#' @param remember a logical. Whether to keep a copy of the context and its
#'    contents throughout each stage for debugging purposes--this makes it
#'    easy to go back and investigate a stage. This could be optimized by
#'    developing a package for "diffing" two environments. The default is
#'    \code{FALSE}.
stageRunner__initialize <- function(context, stages, remember = FALSE) {
  context <<- context
  stages <<- stages
  stopifnot(all(vapply(stages, is.function, logical(1))))
  remember <<- remember
}

#' Run the stages in a stageRunner object.
#'
#' @param stage_key character. A string representing the stage to run
#'   (or re-run). This can be a regular expression. The default is \code{NULL},
#'   which runs the whole sequences of stages. If the character is of
#'   length greater than 1, all matching stages will attempt to be run.
stageRunner__run <- function(stage_key = NULL) {
  active_stages <- if (missing(stage_key) || is.null(stage_key)) {
    seq_along(stages)
  } else {
    if (is.logical(stage_key)) {
      stopifnot(length(stage_key) == length(stages))
      seq_along(stages)[stage_key]
    } else {
      seqs <- seq_along(stages)
      vapply(stage_key, function(key) {
        if (is.numeric(key) && abs(key) %in% seqs) as.integer(key)
        else if (is.character(key)) {
          finds <- grepl(key, names(stages))
          if (length(finds) == 0) stop("No stage with key '", key, "' found")
          else if (sum(finds) > 1)
            stop("Multiple stages with key '", key, "', found: ",
                 names(stages)[finds])
          else which(finds)
        } else stop("Invalid stage key")
      }, integer(1))
    }
  } # End active_stages assignment

  # Now that we have determined which stages to run, cycle through them all.
  # It is up to the user to determine that context changes make sense.
  # We also sort the stages to ensure linearity is preserved. Stagerunner
  # enforces the linearity and directionality set in the stage definitions.
  if (TRUE != all.equal(sorted_stages <- sort(active_stages), active_stages))
    warning("Stages ", paste0(active_stages, collapse = ", "), " were ",
            "requested to be run out of order. Coercing them to run in the ",
            "order they were originally defined.")
  
  lapply(stages[sorted_stages], function(stage) stage(context))
  TRUE
}

#' Stage runner is a reference class for parametrizing and executing
#' a linear sequence of actions.
#' 
#' @docType class 
#' @name stageRunner
stageRunner <- setRefClass('stageRunner',
  fields = list(context = 'environment', stages = 'list', remember = 'logical'),
  methods = list(
    initialize = stageRunner__initialize,
    run        = stageRunner__run
  )

)


