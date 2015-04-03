accessor_method <- function(attr) {
  fn <- eval(bquote(
    function(`*VALUE*` = NULL)
      if (missing(`*VALUE*`)) .(substitute(attr))
      else .(substitute(attr)) <<- `*VALUE*`
  ))
  environment(fn) <- parent.frame()
  fn
}

#' Initialize a stageRunner object.
#'
#' stageRunner objects are used for executing a linear sequence of
#' actions on a context (an environment). For example, if we have an
#' environment \code{e} containing \code{x = 1, y = 2}, then using
#' \code{stages = list(function(e) e$x <- e$x + 1, function(e) e$y <- e$y - e$x)}
#' will cause \code{x = 2, y = 0} after running the stages.
#'
#' @name stageRunner_initialize
#' @param context an environment. The initial environment that is getting
#'    modified during the execution of the stages. 
#' @param .stages a list. The functions to execute on the \code{context}.
#' @param remember a logical. Whether to keep a copy of the context and its
#'    contents throughout each stage for debugging purposes--this makes it
#'    easy to go back and investigate a stage. This could be optimized by
#'    developing a package for "diffing" two environments. The default is
#'    \code{FALSE}. When set to \code{TRUE}, the return value of the
#'    \code{run} method will be a list of two environments: one of what
#'    the context looked like before the \code{run} call, and another
#'    of the aftermath.
#' @param mode character. Controls the default behavior of calling the
#'    \code{run} method for this stageRunner. The two supported options are
#'    "head" and "next". The former gives a stageRunner which always begins
#'    from the first stage if the \code{from} parameter to the \code{run}
#'    method is blank. Otherwise, it will begin from the previous unexecuted
#'    stage.  The default is "head". This argument has no effect if
#'    \code{remember = FALSE}.
stageRunner_initialize <- function(context, .stages, remember = FALSE,
                                    mode = getOption("stagerunner.mode") %||% 'head') {
  # We must do our own type checking on context for compatibility with
  # objectdiff::tracked_environment.
  if (!is.environment(context)) {
    stop("Please pass an ", sQuote("environment"), " as the context for ",
         "a stageRunner")
  }

  self$.parent <- structure(NULL, class = "uninitializedField")
  self$.finished <- FALSE # TODO: Remove this hack for printing
  self$.context <- context

  if (identical(remember, TRUE) && !(is.character(mode) &&
      any((self$.mode <- tolower(mode)) == c('head', 'next')))) {
    stop("The mode parameter to the stageRunner constructor must be ",
         "either 'head' or 'next'.")
  }

  if (length(.stages) == 0) {
    warning("stageRunners with zero stages may cause problems.", .call = FALSE)
  }

  if (!is_pre_stagerunner(.stages)) {
    stop("Can only turn a function or list of functions into a stageRunner", call. = FALSE)
  }

  if (is.function(.stages)) .stages <- list(.stages)
  self$stages <- .stages

  # Construct recursive stagerunners out of a list of lists.
  for (i in seq_along(self$stages))
    if (is.list(self$stages[[i]]))
      self$stages[[i]] <-
        stageRunner$new(self$.context, self$stages[[i]], remember = remember)
    else if (is.function(self$stages[[i]]) || is.null(self$stages[[i]]))
      self$stages[[i]] <- stageRunnerNode$new(self$stages[[i]], self$.context)

  # Do not allow the '/' character in stage names, as it's reserved for
  # referencing nested stages.
  if (any(violators <- grepl('/', names(self$stages), fixed = TRUE))) {
    msg <- paste0("Stage names may not have a '/' character. The following do not ",
      "satisfy this constraint: '",
      paste0(names(self$stages)[violators], collapse = "', '"), "'")
    stop(msg)
  }

  self$remember <- remember
  if (isTRUE(self$remember)) {
    # Set up parents for treeSkeleton.
    self$.clear_cache()
    self$.set_parents()
    if (self$with_tracked_environment()) {
      self$.set_prefixes()
    } else if (length(self$stages) > 0) {
      # Set the first cache environment
      first_env <- treeSkeleton$new(self$stages[[1]])$first_leaf()$object
      first_env$cached_env <- new.env(parent = parent.env(self$.context))
      copy_env(first_env$cached_env, self$.context)
    }
  }
}

#' Run the stages in a stageRunner object.
#'
#' @name stageRunner_run
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
stageRunner_run <- function(from = NULL, to = NULL,
                             normalized = FALSE, verbose = FALSE,
                             remember_flag = TRUE, mode = .mode, .depth = 1, ...) {
  if (identical(normalized, FALSE)) {
    if (missing(from) && identical(self$remember, TRUE) && identical(mode, 'next')) {
      from <- self$next_stage()
      if (missing(to)) to <- TRUE
    }
    stage_key <- normalize_stage_keys(from, stages, to = to)
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
        if (!is.stagerunner(stages[[stage_index]])) {
          stop("Invalid stage key: attempted to make a nested stage reference ",
               "to a non-existent stage")
        }

        function(...) {
          stages[[stage_index]]$run(stage_key[[stage_index]], normalized = TRUE,
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
    else if (remember) { run_stage(..., remember_flag = FALSE) }
    else { run_stage(...) }

    if (remember && !nested_run) {
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

  if (remember && remember_flag) { list(before = before_env, after = self$.context) }
  else { invisible(TRUE) }
}

#' Wrap a function around a stageRunner's terminal nodes
#'
#' If we want to execute some behavior just before and just after executing
#' terminal nodes in a stageRunner, a solution without this method would be
#' to overlay two runners -- one before and one after. However, this is messy,
#' so this function is intended to replace this approach with just one function.
#'
#' Consider the runner
#'   \code{sr <- stageRunner$new(some_env, list(a = function(e) print('2'))}
#' If we run 
#'   \code{sr2 <- stageRunner$new(some_env, list(a = function(e) {
#'     print('1'); yield(); print('3') }))
#'    sr1$around(sr2)
#'    sr1$run()
#'  }
#' then we will see 1, 2, and 3 printed in succession. The \code{yield()}
#' keyword is used to specify when to execute the terminal node that
#' is sandwiched in the "around" runner.
#'
#' @name stageRunner_around
#' @param other_runner stageRunner. Another stageRunner from which to create
#'   an around procedure. Alternatively, we could give a function or a list
#'   of functions.
stageRunner_around <- function(other_runner) {
  if (is.null(other_runner)) return(self)
  if (!is.stagerunner(other_runner)) {
    other_runner <- stageRunner$new(self$.context, other_runner)
  }

  stagenames <- names(other_runner$stages) %||% rep("", length(other_runner$stages))
  lapply(seq_along(other_runner$stages), function(stage_index) {
    name <- stagenames[stage_index]
    this_index <- 
      if (identical(name, "")) stage_index
      else if (is.element(name, names(stages))) name
      else return()

    if (is.stagerunner(self$stages[[this_index]]) &&
        is.stagerunner(other_runner$stages[[stage_index]])) {
      self$stages[[this_index]]$around(other_runner$stages[[stage_index]])
    } else if (is.stageRunnerNode(self$stages[[this_index]]) &&
               is.stageRunnerNode(other_runner$stages[[stage_index]])) {
      self$stages[[this_index]]$around(other_runner$stages[[stage_index]])
    } else {
      warning("Cannot apply around stageRunner because ",
              this_index, " is not a terminal node.")
    }
  })
  self
}

#' Coalescing a stageRunner object is taking another stageRunner object
#' with similar stage names and replacing the latter's cached environments
#' with the former's.
#'
#' @name stageRunner_coalesce
#' @param other_runner stageRunner. Another stageRunner from which to coalesce.
#' @note coalescing is ill-defined for stageRunner with unnamed stages,
#'    since it is impossible to tell when a stage has changed.
stageRunner_coalesce <- function(other_runner) {
  # TODO: Should we care about insertion of new stages causing cache wipes?
  # For now it seems like this would just be an annoyance.
  # stopifnot(remember)
  if (!isTRUE(remember)) return()

  if (self$with_tracked_environment()) {
    if (!other_runner$with_tracked_environment()) {
      stop("Cannot coalesce stageRunners using tracked_environments with ",
           "those using vanilla environments", call. = FALSE)
    }

    compare_head <- function(x, y) {
      m <- seq_len(min(length(x), length(y)))
      x[m] != y[m]
    }

    common <- sum(cumsum(compare_head(self$stage_names(), other_runner$stage_names())) == 0)
    # Warning: Coalescing stageRunners with tracked_environments does not
    # duplicate the tracked_environment, so the other_runner becomes invalidated,
    # and this is a destructive action.
    # TODO: (RK) What if the tracked_environment given initially to the stageRunner
    # already has some commits?
    commits     <- package_function("objectdiff", "commits")
    `.context<-` <- function(obj, value) {
      if (is.stagerunner(obj)) {
        obj$.context <- value
        for (stage in obj$stages) { Recall(stage, value) }
      } else if (is.stageRunnerNode(obj)) {
        obj$.context <- value
        if (is.stagerunner(obj$callable)) { Recall(obj$callable, value) }
      }
    }
    self$.context  <- other_runner$.context
    for (stage in self$stages) { .context(stage) <- other_runner$.context }
    other_runner$.context <- new.env(parent = emptyenv())
    commit_count   <- length(commits(self$.context)) 
    mismatch_count <- commit_count - (common + 1)
    if (mismatch_count > 0) {
      package_function("objectdiff", "force_push")(self$.context, commit_count)
      package_function("objectdiff", "rollback")  (self$.context, mismatch_count)
    }
  } else {
    if (other_runner$with_tracked_environment()) {
      stop("Cannot coalesce stageRunners using vanilla environments with ",
           "those using tracked_environments", call. = FALSE)
    }

    stagenames <- names(other_runner$stages) %||% character(length(other_runner$stages))
    lapply(seq_along(other_runner$stages), function(stage_index) {
      # TODO: Match by name *OR* index
      if (stagenames[[stage_index]] %in% names(self$stages)) {
        # If both are stageRunners, try to coalesce our sub-stages.
        if (is.stagerunner(stages[[names(self$stages)[stage_index]]]) &&
            is.stagerunner(other_runner$stages[[stage_index]])) {
            stages[[names(self$stages)[stage_index]]]$coalesce(
              other_runner$stages[[stage_index]])
        # If both are not stageRunners, copy the cached_env if and only if
        # the stored function and its environment are identical
        } else if (!is.stagerunner(stages[[names(self$stages)[stage_index]]]) &&
            !is.stagerunner(other_runner$stages[[stage_index]]) &&
            !is.null(other_runner$stages[[stage_index]]$cached_env) #&&
            #identical(deparse(stages[[names(stages)[stage_index]]]$fn),
            #          deparse(other_runner$stages[[stage_index]]$fn)) # &&
            # This is way too tricky and far beyond my abilities..
            #identical(stagerunner:::as.list.environment(environment(stages[[names(stages)[stage_index]]]$fn)),
            #          stagerunner:::as.list.environment(environment(other_runner$stages[[stage_index]]$fn)))
            ) {
          self$stages[[names(self$stages)[stage_index]]]$cached_env <-
            new.env(parent = parent.env(self$.context))
          if (is.environment(other_runner$stages[[stage_index]]$cached_env) &&
              is.environment(self$stages[[names(stages)[stage_index]]]$cached_env)) {
            copy_env(self$stages[[names(self$stages)[stage_index]]]$cached_env,
                     other_runner$stages[[stage_index]]$cached_env)
            self$stages[[names(self$stages)[stage_index]]]$executed <- 
              other_runner$stages[[stage_index]]$executed
          }
        }
      }
    })
    self$.set_parents()
  }
  self
}

#' Overlaying a stageRunner object is taking another stageRunner object
#' with similar stage names and adding the latter's stages as terminal stages
#' to the former (for example, to support tests).
#'
#' @name stageRunner_overlay
#' @param other_runner stageRunner. Another stageRunner from which to overlay.
#' @param label character. The label for the overlayed stageRunner. This refers
#'    to the name the former will get wrapped with when appended to the
#'    stages of the current stageRunner. For example, if \code{label = 'test'},
#'    and a current terminal node is unnamed, it will becomes
#'    \code{list(current_node, test = other_runner_node)}.
#' @param flat logical. Whether to use the \code{stageRunner$append} method to
#'    overlay, or simply overwrite the given \code{label}. If \code{flat = TRUE},
#'    you must supply a \code{label}. The default is \code{flat = FALSE}.
stageRunner_overlay <- function(other_runner, label = NULL, flat = FALSE) {
  stopifnot(is.stagerunner(other_runner))
  for (stage_index in seq_along(other_runner$stages)) {
    name <- names(other_runner$stages)[[stage_index]]
    index <-
      if (identical(name, '') || identical(name, NULL)) stage_index
      else if (name %in% names(self$stages)) name
      else stop('Cannot overlay because keys do not match')
    self$stages[[index]]$overlay(other_runner$stages[[stage_index]], label, flat)
  }
  TRUE
}

#' Transform the callable's of the terminal nodes of a stageRunner.
#'
#' Every terminal node in a stageRunner is of type stageRunnerNode.
#' These each have a callable, and this method transforms those
#' callables in the way given by the first argument.
#'
#' @name stageRunner_transform
#' @param transformation function. The function which transforms one callable
#'   into another.
stageRunner_transform <- function(transformation) {
  for (stage_index in seq_along(self$stages))
    self$stages[[stage_index]]$transform(transformation)
}

#' Append one stageRunner to the end of another.
#'
#' @name stageRunner_append
#' @param other_runner stageRunner. Another stageRunner to append to the current one.
#' @param label character. The label for the new stages (this will be the name of the
#'   newly appended list element).
stageRunner_append <- function(other_runner, label = NULL) {
  stopifnot(is.stagerunner(other_runner))
  new_stage <- structure(list(other_runner), names = label)
  self$stages <- base::append(self$stages, new_stage)
  TRUE
}

#' Retrieve a flattened list of canonical stage names for a stageRunner object
#'
#' For example, if we have stages
#'   \code{stages = list(a = list(b = 1, c = 2), d = 3, e = list(f = 4, g = 5))}
#' then this method would return
#'   \code{list('a/b', 'a/c', 'd', 'e/f', 'e/g')}
#'
#' @name stageRunner_stage_names
#' @return a list of canonical stage names.
## @examples
## f <- function() {}
## sr <- stageRunner$new(new.env(),
##   list(a = stageRunner$new(new.env(), list(b = f, c = f)), d = f,
##   e = stageRunner$new(new.env(), list(f = f, g = f))))
## sr$stage_names()
stageRunner_stage_names <- function() {
  nested_stages <- function(x) if (is.stagerunner(x)) nested_stages(x$stages) else x
  nested_names(lapply(self$stages, nested_stages))
}

#' For stageRunners with caching, find the next unexecuted stage.
#'
#' @name stageRunner_next_stage
#' @return a character stage key giving the next unexecuted stage.
#'   If all stages have been executed, this returns \code{FALSE}.
#'   If the stageRunner does not have caching enabled, this will
#'   always return the first stage key (`'1'`).
stageRunner_next_stage <- function() {
  for (stage_index in seq_along(self$stages)) {
    is_unexecuted_terminal_node <- is.stageRunnerNode(self$stages[[stage_index]]) &&
      !self$stages[[stage_index]]$was_executed()
    has_unexecuted_terminal_node <- is.stagerunner(self$stages[[stage_index]]) &&
      is.character(tmp <- self$stages[[stage_index]]$next_stage())

    if (is_unexecuted_terminal_node) return(as.character(stage_index))
    else if (has_unexecuted_terminal_node)
      return(paste(c(stage_index, tmp), collapse = '/'))
  }
  FALSE
}

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
    else if (is.stageRunnerNode(stage)) !is.null(stage$cached_env)
    else FALSE

  lapply(seq_along(stage_names), function(index) {
    prefix <- paste0(rep('  ', (if (is.numeric(indent)) indent else 0) + 1), collapse = '')
    marker <-
      if (self$remember && began_stage(self$stages[[index]])) {
        next_stage <- treeSkeleton$new(stages[[index]])$last_leaf()$successor()$object
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

#' Whether or not the stageRunner has a key matching this input.
#'
#' @param key ANY. The potential key.
#' @return \code{TRUE} or \code{FALSE} accordingly.
stageRunner_has_key <- function(key) {
  has <- tryCatch(normalize_stage_keys(key, self$stages), error = function(.) FALSE)
  any(c(has, recursive = TRUE))
}

#' Clear all caches in this stageRunner, and recursively.
#' @name stageRunner_.clear_cache
stageRunner_.clear_cache <- function() {
  for (i in seq_along(self$stages)) {
    if (is.stagerunner(self$stages[[i]])) self$stages[[i]]$.clear_cache()
    else self$stages[[i]]$cached_env <- NULL
  }
  TRUE
}

#' Set all parents for this stageRunner, and recursively
#' @name stageRunner_.set_parents
stageRunner_.set_parents <- function() {
  for (i in seq_along(self$stages)) {
    # Set convenience helper attribute "child_index" to ensure that treeSkeleton
    # can find this stage.
    if (is.refClass(self$stages[[i]])) {
      # http://stackoverflow.com/questions/22752021/why-is-r-capricious-in-its-use-of-attributes-on-reference-class-objects
      unlockBinding('self', attr(self$stages[[i]], '.xData'))
      attr(attr(self$stages[[i]], '.xData')$self, 'child_index') <<- i
      lockBinding('self', attr(self$stages[[i]], '.xData'))
    } else attr(self$stages[[i]], 'child_index') <<- i

    if (!is.refClass(self$stages[[i]])) {
      attr(self$stages[[i]], 'parent') <<- self
    } else {
      # if stages[[i]] has a .set_parents method (e.g. it is a stagerunner), run that
      if ('.set_parents' %in% ls(slef$stages[[i]]$.refClassDef@refMethods, all.names = TRUE))
        self$stages[[i]]$.set_parents()
      self$stages[[i]]$parent(self)
    }
  }
  self$.parent <- NULL
}

#' Get an environment representing the context directly before executing a given stage.
#'
#' @note If there is a lot of data in the remembered environment, this function
#'   may be computationally expensive as it has to create a new environment
#'   with a copy of all the relevant data.
#' @param stage_index integer. The substage for which to grab the before
#'   environment.
#' @return a fresh new environment representing what would have been in
#'   the context as of right before the execution of that substage.
stageRunner_.before_env <- function(stage_index) {
  cannot_run_error <- function() {
    stop("Cannot run this stage yet because some previous stages have ",
         "not been executed.")
  }

  if (self$with_tracked_environment()) {
    # We are using the objectdiff package and its tracked_environment,
    # so we have to "roll back" to a previous commit.
    current_commit <- paste0(self$.prefix, stage_index)

    if (!current_commit %in% names(package_function("objectdiff", "commits")(self$.context))) {
      if (`first_commit?`(current_commit)) {
        # TODO: (RK) Do this more robustly. This will fail if there is a 
        # first sub-stageRunner with an empty list as its stages.
        package_function("objectdiff", "commit")(self$.context, current_commit)
      } else {
        cannot_run_error()
      }
    } else {
      package_function("objectdiff", "force_push")(self$.context, current_commit)
    }

    env <- new.env(parent = package_function("objectdiff", "parent.env.tracked_environment")(self$.context))
    copy_env(env, package_function("objectdiff", "environment")(self$.context))
    env
  } else {
    env <- self$stages[[stage_index]]$cached_env
    if (is.null(env)) { cannot_run_error() }

    # Restart execution from cache, so set context to the cached environment.
    copy_env(self$.context, env)
    env
  }
}

#' Mark a given stage as being finished.
#' 
#' @param stage_index integer. The index of the substage in this stageRunner.
stageRunner_.mark_finished <- function(stage_index) {
  node <- treeSkeleton$new(self$stages[[stage_index]])$successor()

  if (!is.null(node)) { # Prepare a cache for the future!
    if (self$with_tracked_environment()) {
      # We assume the head for the tracked_environment is set correctly.
      package_function("objectdiff", "commit")(self$.context, node$object$index())
    } else {
      node$object$cached_env <- new.env(parent = parent.env(self$.context))
      copy_env(node$object$cached_env, self$.context)
    }
  } else {
    # TODO: Remove this hack used for printing
    root <- self$.root()
    root$.finished <- TRUE
  }
}

#' Determine the root of the stageRunner.
#'
#' @name stageRunner_.root
#' @return the root of the stageRunner
stageRunner_.root <- function() {
  treeSkeleton$new(self)$root()$object
}

#' @export
print.stageRunner <- function(x, ...) {
  x$show(...)
}

#' @export
print.stageRunnerNode <- function(x, ...) {
  x$show(...)
}

stageRunner_ <- R6::R6Class('stageRunner',
  active = list(context = function() self$.context),                            
  public = list(
    .context = NULL,
    stages = list(),
    remember = FALSE,
    .mode = "head",
    .parent = NULL,
    .finished = FALSE,
    .prefix = "",
    initialize   = stageRunner_initialize,
    run          = stageRunner_run,
    around       = stageRunner_around,
    coalesce     = stageRunner_coalesce,
    overlay      = stageRunner_overlay,
    transform    = stageRunner_transform,
    append       = stageRunner_append,
    stage_names  = stageRunner_stage_names,
    parent       = accessor_method(.parent),
    children     = function() { stages },
    next_stage   = stageRunner_next_stage,
    show         = stageRunner_show,
    has_key      = stageRunner_has_key,
    mode         = accessor_method(.mode),
    .set_parents = stageRunner_.set_parents,
    .clear_cache = stageRunner_.clear_cache,
    .root        = stageRunner_.root,

    # objectdiff intertwined functionality
    .set_prefixes  = stageRunner_.set_prefixes,
    .before_env    = stageRunner_.before_env,
    .mark_finished = stageRunner_.mark_finished,
    with_tracked_environment = function() { is(.context, 'tracked_environment') }
  )
)

#' @export
stageRunner <- structure(
  function(...) { stageRunner_$new(...) },
  class = "stageRunner_"
)

#' @export
`$.stageRunner_` <- function(...) {
  stopifnot(identical(..2, "new"))
  ..1
}

#' Stagerunner nodes are environment wrappers around individual stages
#' (i.e. functions) in order to track meta-data (e.g., for caching).
#' 
#' @param fn function. This will be wrapped in an environment.
#' @param parent_obj stageRunner. The enclosing stageRunner object.
#' @param parent_env environment. The parent environment of the created
#'   \code{stageRunnerNode} object. The default is the calling
#'   environment (i.e., \code{parent.frame()}).
#' @return an environment with some additional attributes for
#'   navigating in a tree-like structure.
#' @name stageRunnerNode
#' @docType class
stageRunnerNode_ <- R6::R6Class('stageRunnerNode',
  public = list(
    callable = NULL,
    cached_env = NULL,
    .context = NULL,
    .parent = NULL,
    executed = FALSE,
    initialize = function(.callable, .context = NULL) {
      stopifnot(is_any(.callable, c('stageRunner', 'function', 'NULL')))
      self$callable <- .callable
      self$.context <- .context
      self$executed <- FALSE
    },
    run = function(..., .cached_env = NULL, .callable = self$callable) {
      # TODO: Clean this up by using environment injection utility fn
      correct_cache <- .cached_env %||% self$cached_env
      if (is.null(.callable)) FALSE
      else if (is.stagerunner(.callable))
        .callable$run(..., .cached_env = correct_cache)
      else {
        tmp <- new.env(parent = environment(.callable))
        environment(.callable) <- tmp
        environment(.callable)$cached_env <- correct_cache
        on.exit(environment(.callable) <- parent.env(environment(.callable)))
        .callable(self$.context, ...)
      }
      self$executed <- TRUE
    }, 

    # This function goes hand in hand with stageRunner$around
    around = function(other_node) {
      if (is.stageRunnerNode(other_node)) other_node <- other_node$callable
      if (is.null(other_node)) return(FALSE)
      if (!is.function(other_node)) {
        warning("Cannot apply stageRunner$around in a terminal ",
                "node except with a function. Instead, I got a ",
                class(other_node)[1])
        return(FALSE)
      }

      new_callable <- other_node
      # Inject yield() keyword
      yield_env <- new.env(parent = environment(new_callable))
      yield_env$.parent_context <- self
      yield_env$yield <- function() {
        # ... lives up two frames, but the run function lives up 1,
        # so we have to do something ugly
        run <- eval.parent(quote(.parent_context$run))
        args <- append(eval.parent(quote(list(...)), n = 2),
          list(.callable = callable))
        do.call(run, args, envir = parent.frame())
      }
      environment(yield_env$yield) <- new.env(parent = baseenv())
      environment(yield_env$yield)$callable <- self$callable

      environment(new_callable) <- yield_env
      self$callable <- new_callable
      TRUE
    },

    overlay = function(other_node, label = NULL, flat = FALSE) {
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
    },
    transform = function(transformation) {
      if (is.stagerunner(self$callable)) self$callable$transform(transformation)
      else self$callable <- transformation(self$callable)
    },
    was_executed = function() { self$executed },
    parent   = function() { attr(self, "parent") }, # accessor_method(.parent),
    children = function() list(),
    show     = function() { cat("A stageRunner node containing: \n"); print(self$callable) },

    # Functions which intertwine with the objectdiff package
    index    = function() {
      ix <- which(vapply(attr(self, "parent")$stages,
        function(x) identical(self, x), logical(1)))
      paste0(attr(self, "parent")$.prefix, ix)
    }
  )
)

#' @export
stageRunnerNode <- structure(
  function(...) { stageRunnerNode_$new(...) },
  class = "stageRunnerNode_"
)

#' @export
`$.stageRunnerNode_` <- function(...) {
  stopifnot(identical(..2, "new"))
  ..1
}


#' Check whether an R object is a stageRunner object
#'
#' @export
#' @param obj any object.
#' @return \code{TRUE} if the object is of class
#'    \code{stageRunner}, \code{FALSE} otherwise.
is.stagerunner <- function(obj) inherits(obj, 'stageRunner')
#' @export
is.stageRunner <- is.stagerunner
is.stageRunnerNode <- function(obj) inherits(obj, 'stageRunnerNode')



