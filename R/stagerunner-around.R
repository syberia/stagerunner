## The `around` method on a stagerunner is used as sort of [setup and teardown](http://stackoverflow.com/questions/6854658/explain-the-setup-and-teardown-methods-used-in-test-cases)
## hooks on arbitrary stages.
## 
## For example, imagine we have a stagerunner that looks like the following.
##
##   * import data
##   * clean data
##      * impute variable 1
##      * discretize variable 2
##
## Imagine we want to write test functions that ensure the correct behavior
## is happening during the data cleaning. We can write a stagerunner with
## an identical tree structure that performs additional testing to ensure
## our work is correct:
##
## ```r
## new_runner <- stageRunner$new(new.env(), list(
##   "import data" = function(e) { yield(); stopifnot(!is.null(e$data)) },
##   "clean data"  = list(
##     "impute variable 1" = function(e) {
##       yield()
##       stopifnot(!any(is.na(e$data$variable1)))
##     }, "discretize variable 2" = function(e) {
##       yield()
##       stopifnot(is.factor(e$data$variable2))
##    })
## ))
## ```
##
## The keyword `yield` is injected into a stagerunner that is used with the
## `around` method, and means "execute the stage of the stagerunner that is
## being wrapped that would normally occur at this point." Code before
## and after the `yield` keyword can be used to perform additional assertions
## about what happened during the execution of the stage.
##
## ```r
## runner$around(new_runner)
## runner$run()
## ```
##
## If any of the above assertions fail, we will now get an error.
##
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
      else if (is.element(name, names(self$stages))) name
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

