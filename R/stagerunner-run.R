## The heart of a stagerunner object is its `run` method, depicted on the
## right. A stagerunner consists of two things:
##
##  * __a context__: This is an [environment](http://adv-r.had.co.nz/Environments.html) object
##    that allows the user to persistently store information between stages.
##    The usual way to build a data pipeline is to provide functions with
##    various inputs and hook them up to functions with various outputs.
##    
##    This is nice because it is clear what the inputs and outputs will be.
##    However, the disadvantage is that hooking up all the functions can
##    become pretty messy.
##
##    In this approach, we let the user set their own
##    conventions for what to place in the context. The advantage is that
##    *all stages have the same form*, a function taking one argument (the 
##    context), and so they become easy to manipulate.
##
##  * __stages__: A list of functions or, recursively, other stagerunners. Each function
##    should take precisely one argument: the *context* described above.
##    If you have some familiarity with pure mathematics, you will know the
##    [original inspiration](http://en.wikipedia.org/wiki/Group_action) for stagerunners:
##    a stagerunner is a sequence of actions on an environment.
## 
## *Running* a portion of a stagerunner means to execute some of its stages on 
## its context. For example, suppose we start with an empty environment
## `context = new.env()` and the following stages:
##
## ```r
## context <- new.env()
## runner  <- stagerunner(context, list(
##  "Set x"    = function(e) { e$x <- 1 },
##  "Double x" = function(e) { e$x <- 2 * e$x }
## ))
## ```
##
## If we write `runner$run("Set x")`, then `context$x` will become `1`.
## If we write `runner$run(2)` (a syntactical shortcut), then `context$x`
## becomes `2`. If we write `runner$run(2)` again, it will become `4`.
##
## The real advantage of this approach becomes clear when we enable the `remember`
## flag:
##
## ```r
## context <- new.env()
## runner  <- stagerunner(remember = TRUE, context, list(
##   "Import data"               = function(e) e$data <- iris,
##   "Create dependent variable" = function(e) e$dep_var <- e$data[[1]] > 5,
##   "Create derived variable"   = function(e) e$diff <- e$data[[1]] - e$data[[2]]
## ))
## ```
## 
## Now, the stagerunner holds a copy of the full environment in each stage:
## this means we can re-run previous stages at will.
##
## ```r
## runner$run()        # Run all stages
## runner$data <- NULL # Clear the data
## runner$run(2)       # Re-run just the second stage.
## ```
##
## In this scenario, the `data` gets restored from a cached environment--
## what the context looked like after the first stage finished--
## and we have a `dep_var` column (although no `diff` column since
## the third stage was now "rolled back").
##
## This kind of approach also allows us to debug what happens during execution:
##
## ```r
## envs <- runner$run(2)
## ls(envs$before$data) # The iris attributes
## ls(envs$after$data)  # The iris attributes *and* dep_var
## ```
##
## When a stagerunner is set to remember its progress the output of the `run`
## function consists of a list with keys `before` and `after` representing
## two environments: what the stagerunner's context looked like before
## and after executing that stage.
##
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
#'   Notice that substrings are allowed for characters.
#'   The default is \code{NULL}, which runs the whole sequences of stages.
#' @param to an indexing parameter. If \code{from} refers to a single stage,
#'   attempt to run from that stage to this stage (or, if this one comes first,
#'   this stage to that stage). For example, if we have
#'      \code{stages = list(a = list(b = 1, c = 2), d = 3, e = list(f = 4, g = 5))}
#'   where the numbers are some functions, and we call \code{run} with
#'   \code{from = 'a/c'} and \code{to = 'e/f'}, then we would execute
#'   stages \code{"a/c", "d", "e/f"}.
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
#' @param normalized logical. A convenience recursion performance helper. If
#'   \code{TRUE}, stageRunner will assume the \code{from} argument is a
#'   nested list of logicals.
#' @param .depth integer. Internal parameter for keeping track of nested
#'   execution depth.
#' @param ... Any additional arguments to delegate to the \code{stageRunnerNode}
#'   object that will execute its own \code{run} method.
#'   (See \code{stageRunnerNode$run})
#' @return TRUE or FALSE according as running the stages specified by the
#'   \code{from} and \code{to} keys succeeded or failed. If
#'   \code{remember = TRUE}, this will instead be a list of the environment
#'   before and after executing the aforementioned stages. (This allows
#'   comparing what changes were made to the \code{context} during the
#'   execution of the stageRunner.)
#' @examples
#' env <- new.env()
#' some_fn    <- function(e) e$x <- 1
#' other_fn   <- function(e) e$y <- 1
#' another_fn <- function(e) e$z <- 1
#' sr <- stagerunner(env, list(stage_one =
#'  stagerunner(env, list(substage_one = some_fn, substage_two = other_fn)),
#'  stage_two = another_fn))
#' 
#' # Here, the following all execute only substage_two:
#'
#' sr$run(list(list(FALSE, TRUE), FALSE))
#' sr$run(list(list(1, 2)))
#' sr$run('stage_one/substage_two')
#' sr$run('one/two')
#' stopifnot(is.null(env$z), is.null(env$x), identical(env$y, 1))
#'
#' # This will execute all but "stage_one" (i.e., only "stage_two")
#' sr$run(-1)
#' stopifnot(identical(env$z, 1))
run <- function(from = NULL, to = NULL, verbose = FALSE, remember_flag = TRUE,
                mode = self$.mode, normalized = FALSE, .depth = 1, ...) {
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
