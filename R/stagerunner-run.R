#' Run the stages in a stageRunner object.
#'
#' @param from an indexing parameter. Many forms are accepted, but the
#'   easiest is the name of the stage. For example, if we have
#'   \code{stageRunner$new(context, list(stage_one = some_fn, stage_two = some_other_fn))}
#'   then using \code{run('stage_one')} will execute \code{some_fn}.
#'   Additional indexing forms are logical (which stages to execute),
#'   numeric (which stages to execute by indices), negative (all but the
#'   given stages), character (as above), and nested forms of these.
#'   The latter refers to instances of the following:
#'   \code{stageRunner$new(context, list(stage_one =
#'     stageRunner$new(context, substage_one = some_fn, substage_two = other_fn),
#'     stage_two = another_fn))}.
#'   Here, the following all execute only substage_two:
#'   \code{run(list(list(FALSE, TRUE), FALSE))},
#'   \code{run(list(list(1, 2)))},
#'   \code{run('stage_one/substage_two')},
#'   \code{run('one/two')},
#'   \code{run(list(list('one', 'two')))},
#'   \code{run(list(list('one', 2)))}
#'   Notice that regular expressions are allowed for characters.
#'   The default is \code{NULL}, which runs the whole sequences of stages.
#' @param to an indexing parameter. If \code{stage_key} refers to a single stage,
#'   attempt to run from that stage to this stage (or, if this one comes first,
#'   this stage to that stage). For example, if we have
#'      \code{stages = list(a = list(b = 1, c = 2), d = 3, e = list(f = 4, g = 5))}
#'   where the numbers are some functions, and we call \code{run} with
#'   \code{stage_key = 'a/c'} and \code{to = 'e/f'}, then we would execute
#'   stages \code{"a/c", "d", "e/f"}.
#' @param normalized logical. A convenience recursion performance helper. If
#'   \code{TRUE}, stageRunner will assume the \code{stage_key} argument is a
#' @param verbose logical. Whether or not to display pretty colored text
#'   informing about stage progress.
#'   nested list of logicals.
#' @param remember_flag logical. An internal argument used by \code{run}
#'   recursively if the \code{stageRunner} object has the \code{remember}
#'   field set to \code{TRUE}. If \code{remember_flag} is FALSE, \code{run}
#'   will not attempt to restore the context from cache (e.g., if we are
#'   executing five stages simultaneously with \code{remember = TRUE},
#'   the first stage's context should be restored from cache but none
#'   of the remaining stages should).
#' @param mode character. If \code{mode = 'head'}, then by default the
#'   \code{from} parameter will be used to execute that stage and that
#'   stage only. If \code{mode = 'next'}, then the \code{from} parameter
#'   will be used to run (by default, if \code{to} is left missing)
#'   from the last successfully executed stage to the stage given by
#'   \code{from}. If \code{from} occurs before the last successfully
#'   executed stage (say S), the stages will be run from \code{from} to S.
#' @param .depth integer. Internal parameter for keeping track of nested running level.
#' @param ... Any additional arguments to delegate to the \code{stageRunnerNode}
#'   object that will execute its own \code{run} method.
#'   (See \code{stageRunnerNode$run})
#' @return TRUE or FALSE according as running the stages specified by the
#'   \code{stage_key} succeeded or failed.  If \code{remember = TRUE},
#'   this will instead be a list of the environment before and after
#'   executing the aforementioned stages. (This allows comparing what
#'   changes were made to the \code{context} during the execution of
#'   the stageRunner.
run <- function(from = NULL, to = NULL,
                             normalized = FALSE, verbose = FALSE,
                             remember_flag = TRUE, mode = self$.mode, .depth = 1, ...) {
  if (identical(normalized, FALSE)) {
    if (missing(from) && identical(self$remember, TRUE) && identical(mode, 'next')) {
      from <- self$next_stage()
      if (missing(to)) to <- TRUE
    }
    stage_key <- normalize_stage_keys(from, self$stages, to = to)
  } else stage_key <- from

  # Now that we have determined which stages to run, cycle through them all.
  # It is up to the user to determine that context changes make sense.
  # We also implicitly sort the stages to ensure linearity is preserved.
  # Stagerunner enforces the linearity and directionality set in the stage definitions.
  
  # If we are remembering changes, recall what the environment looked like
  # *before* we ran anything.
  before_env <- NULL

  for (stage_index in seq_along(stage_key)) {
    nested_run <- TRUE
    
    # Determine how to run this stage, depending on whether it is an
    # terminal node or nested stagerunner. We compute this first
    # in case we run into referencing errors (e.g., the requested
    # stage does not exist).
    run_stage <-
      if (identical(stage_key[[stage_index]], TRUE)) {
        stage <- self$stages[[stage_index]]
        if (is.stagerunner(stage)) { 
          function(...) { stage$run(verbose = verbose, .depth = .depth + 1, ...) }
        } else {
         nested_run <- FALSE
         # Intercept the remember_flag argument to calls to the stageRunnerNode
         # (since it doesn't know how to use it).
         function(..., remember_flag = TRUE) { stage$run(...) }
        }
      } else if (is.list(stage_key[[stage_index]])) {
        if (!is.stagerunner(self$stages[[stage_index]])) {
          stop("Invalid stage key: attempted to make a nested stage reference ",
               "to a non-existent stage")
        }

        function(...) {
          self$stages[[stage_index]]$run(stage_key[[stage_index]], normalized = TRUE,
                                    verbose = verbose, .depth = .depth + 1, ...)
        }
      } else next 

    display_message <- verbose && contains_true(stage_key[[stage_index]])
    if (display_message) {
      show_message(names(self$stages), stage_index, begin = TRUE,
                   nested = nested_run, depth = .depth)
    }

    # Now handle when remember = TRUE, i.e., we have to cache the
    # progress along each stage.

    if (self$remember && remember_flag && is.null(before_env)) {
      # If remember = remember_flag = TRUE and before_env has not been set
      # this is the first stage of a $run() call, so use the cached
      # environment.
      if (nested_run) {
        before_env <- run_stage(..., remember_flag = TRUE)$before
      } else { # a leaf / terminal node
        before_env <- self$.before_env(stage_index)
      }
      
      # If terminal node, execute the stage (if it was nested,  it's already been
      # executed in order to recursively fetch the before_env).
      if (!nested_run) { run_stage(...) }
    }
    else if (self$remember) { run_stage(..., remember_flag = FALSE) }
    else { run_stage(...) }

    if (self$remember && !nested_run) {
      # When we're done running a stage (i.e., processing a terminal node),
      # set the cache on the successor node to be the current context
      # (since that node will execute starting with what's in the context now --
      # this also ensures that running that node with a separate call to
      # $run will not bump into a "you haven't executed this stage yet" error).
      self$.mark_finished(stage_index)
    }

    if (display_message) {
      show_message(names(self$stages), stage_index, begin = FALSE,
                   nested = nested_run, depth = .depth)
    }
  }

  if (self$remember && remember_flag) { list(before = before_env, after = self$.context) }
  else { invisible(TRUE) }
}
