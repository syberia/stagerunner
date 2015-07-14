#' Initialize a stageRunner object.
#'
#' stageRunner objects are used for executing a linear sequence of
#' actions on a context (an environment). For example, if we have an
#' environment \code{e} containing \code{x = 1, y = 2}, then using
#' \code{stages = list(function(e) e$x <- e$x + 1, function(e) e$y <- e$y - e$x)}
#' will cause \code{x = 2, y = 0} after running the stages.
#'
#' @name stageRunner_initialize
#' @param context environment. The initial environment that is getting
#'    modified during the execution of the stages. 
#' @param stages list. The functions to execute on the \code{context}.
#' @param remember logical. Whether to keep a copy of the context and its
#'    contents throughout each stage for debugging purposes--this makes it
#'    easy to go back and investigate a stage.
#'    
#'    The default is \code{FALSE}. When set to \code{TRUE}, the return value
#'    of the \code{run} method will be a list of two environments: one of what
#'    the context looked like before the \code{run} call, and another
#'    of the aftermath.
## When a stagerunner object is initialized, it needs to convert a
## pre-stagerunner, like
##
## ```r
## list(first = some_function, second = list(
##   sub1 = another_function, sub2 = a_third_function
## )
## ```
## 
## into a stagerunner object. This class constructor will turn the above
## into a hierarchy of stagerunners to make it easier to recursively
## re-use functionality.
#' @param mode character. Controls the default behavior of calling the
#'    \code{run} method for this stageRunner. The two supported options are
#'    "head" and "next". The former gives a stageRunner which always begins
#'    from the first stage if the \code{from} parameter to the \code{run}
#'    method is blank. Otherwise, it will begin from the previous unexecuted
#'    stage. The default is "head". This argument has no effect if
#'    \code{remember = FALSE}.
stagerunner_initialize <- function(context, stages, remember = FALSE,
                                   mode = getOption("stagerunner.mode") %||% "head") {
  
  ## As a convenient shortcut, if a stagerunner is initialized without a second
  ## argument but with a first argument that can be turned into stages, we 
  ## create a new environment for the context.
  if (missing(stages) && !missing(context) && is_pre_stagerunner(context)) {
    stages  <- context
    ## The only parent environment that makes sense is the calling environment.
    context <- new.env(parent = parent.frame())
  }

  ## The `enforce_type` helper in utils.R will print a nice and colorful error
  ## message if we have initialized our stagerunner with the wrong argument
  ## types.
  enforce_type(context,  "environment", "stagerunner", "context")
  enforce_type(remember, "logical",     "stagerunner", "remember")
  enforce_type(mode,     "character",   "stagerunner", "mode")

  ## [`match.arg`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/match.arg.html)
  ## is a convenient base R helper that will error unless one of a given set of
  ## options is chosen.
  match.arg(mode, c("head", "next"))

  stopifnot(length(remember) == 1)

  self$.parent   <- NULL
  # The .finished flag is used for certain features when printing a stagerunner.
  self$.finished <- FALSE 
  self$.context  <- context
  self$.mode     <- tolower(mode)
  self$remember  <- remember

  ## A stagerunner will recursively be represented using more stagerunners.
  ## This way, we can re-use methods defined on a stagerunner on local 
  ## subsections.
  self$stages    <- initialize_stages(stages, context, remember)
  
  ## We wrap up with some messy initialization in case our stagerunner
  ## intends to remember progress.
  if (isTRUE(self$remember)) {
    initialize_remembrance(self)
  }
}

initialize_stages <- function(stages, context, remember) {
  if (length(stages) == 0) {
    warning("stagerunners with zero stages may cause problems.")
  }

  if (!is_pre_stagerunner(stages)) {
    stop("Can only turn a function or list of functions into a stagerunner.")
  }

  if (is.function(stages)) {
    stages <- list(stages)
  }

  ## A loop is slightly faster than an `lapply` here.
  for (i in seq_along(stages)) {
    if (is.list(stages[[i]])) {
      stages[[i]] <- stagerunner(context, stages[[i]], remember = remember)
    } else if (is.function(stages[[i]]) || is.null(stages[[i]])) {
      stages[[i]] <- stageRunnerNode(stages[[i]], context)
    }
  }

  ## We will be using the `/` character in a special way for running 
  ## stages. For example, if we had a runner such as 
  ##
  ##   * import data
  ##   * clean data
  ##      * impute variable 1
  ##      * discretize variable 2
  ##
  ## we would run the first substage using `runner$run("clean data/impute variable 1")`.
  ## To avoid complications, we prevent the use of slashes in the stage names.
  prevent_stage_name_violators(stages)

  stages
}

prevent_stage_name_violators <- function(stages) {
  if (any(violators <- grepl("/", names(stages), fixed = TRUE))) {
    stop(paste0("Stage names may not have a '/' character. The following do not ",
      "satisfy this constraint: '",
      paste0(names(stages)[violators], collapse = "', '"), "'"))
  }
}

initialize_remembrance <- function(stagerunner) {
  stagerunner$.clear_cache()
  ## We set up some meta-data that will be used to track the 
  ## changes occuring in the stagerunner. See the `treeSkeleton` class
  ## later for more details.
  stagerunner$.set_parents()
  if (stagerunner$with_tracked_environment()) {
    stagerunner$.set_prefixes()
  } else if (length(stagerunner$stages) > 0) {
    ## The very first stage should remember what the context looked like
    ## upon initialization. After all, if a user messed with the context
    ## and later re-runs the stagerunner from scratch, it should remember
    ## what the context looked like *at the time of initialization*.
    first_env <- treeSkeleton$new(stagerunner$stages[[1]])$first_leaf()$object
    first_env$.cached_env <- new.env(parent = parent.env(stagerunner$.context))
    copy_env(first_env$.cached_env, stagerunner$.context)
  }
}

