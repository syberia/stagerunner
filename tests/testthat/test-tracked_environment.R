library(testthatsomemore)
library(objectdiff)

context('tracked_environment')

# The objectdiff package provides the ability to use a single environment
# to store a sequence of changes, with a Git-like ability to "roll back"
# to previous versions of the environment. It does this without extraneous
# memory overhead using clever heuristics.

# If a stageRunner is provided a tracked_environment instead of a regular
# environment, it will assume this behavior into its functioning so that
# if its remember parameter is set to TRUE, it will use the tracked_environment's
# rollback abilities to provide the correct behavior when executing an
# earlier stage.

describe('tracked_environments', {
  test_that('it can initialize a stageRunner using a tracked_environment', {
    assert(stageRunner$new(tracked_environment(), list(), remember = TRUE))
  })

  test_that('it can run a simple example with a tracked_environment', {
    sr <- stageRunner$new(tracked_environment(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 2))
    sr$run()
    expect_equal(sr$context$x, 1)
    expect_equal(sr$context$y, 2)
  })

  test_that('it can re-run a previous stage with a tracked_environment', {
    sr <- stageRunner$new(tracked_environment(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 2))
    sr$run()
    sr$run('a')
    expect_equal(sr$context$x, 1)
    expect_null(sr$context$y)
  })

  describe('duplicate of caching tests with tracked_environments', {
    # A helper function for checking whether copy_env is working properly.
    # See also: http://stackoverflow.com/questions/22675046/transforming-a-nested-environment-into-a-nested-list/22675108#22675108
    nested_env_list <- function(env) {
      out <- as.list(env)
      lapply(out, function(x) if (is.environment(x)) nested_env_list(x) else x)
    }

    test_that('it can initialize a trivial stageRunner', {
      sr <- stageRunner$new(tracked_environment(), list(), remember = TRUE)
      expect_false(is.null(tryCatch(sr$run(), error = function(.) NULL)))
    })

    test_that('it should be able to run a simple double staged stageRunner', {
      sr <- stageRunner$new(tracked_environment(), list(function(x) x$x <- 1, function(y) y$y <- 2), remember = TRUE)
      expect_false(is.null(tryCatch(sr$run(), error = function(.) NULL)))
    })

    test_that('the first cached environment is simply the initial context', {
      tmp <- tracked_environment(); tmp$x <- 1
      tmp$y <- function(z) z + 1; tmp$w <- list(1, iris)
      out <- nested_env_list(package_function("objectdiff", "environment")(tmp))
      sr <- stageRunner$new(tmp, list(force), remember = TRUE)
      expect_identical(nested_env_list(sr$run(1)$before), out)
    })

    test_that('the first cached environment is simply the initial context in a first nested stage', {
      tmp <- tracked_environment(); tmp$x <- 1
      tmp$y <- function(z) z + 1; tmp$w <- list(1, iris)
      sr <- stageRunner$new(tmp, list(list(list(force))), remember = TRUE)
      out <- nested_env_list(package_function("objectdiff", "environment")(tmp))
      expect_identical(nested_env_list(sr$run('1/1/1')$before), out)
    })

    test_that('there are no cached environments on non-first stages before anything is run', {
      sr <- stageRunner$new(tracked_environment(), list(force, force, force), remember = TRUE)
      expect_identical(lapply(sr$stages[-1], `[[`, "cached_env"), list(NULL, NULL))
    })

    test_that('running the first two stages updates the cache for the second stage and third stage', {
      sr <- stageRunner$new(tracked_environment(),
        list(function(env) env$x <- 1, force, force), remember = TRUE)
      sr$run(c(1,2))
      expect_identical(nested_env_list(sr$run(2)$before), list(x = 1))
      expect_identical(nested_env_list(sr$run(3)$before), list(x = 1))
    })

    test_that('we cannot run stages out of order due to caching issues', {
      sr <- stageRunner$new(tracked_environment(), list(list(function(env) env$x <- 1, force), list(function(env) env$y <- 1, force)), remember = TRUE)
      sr$run(1)
      expect_error(sr$run('2/2'), 'Cannot run this stage yet because some previous stages have not been executed')
      sr$run('2/1')
      sr$run('2/2') # it should be runnable now
    })

    test_that('parents and children get set in a stageRunner tree with caching', {
      sr <- stageRunner$new(tracked_environment(), list(force, list(force, force)), remember = TRUE)
      expect_equal(length(sr$children()), 2)
      expect_equal(length(sr$children()[[2]]$children()), 2)
    })

    test_that('the environment gets restored from cache upon second execution', {
      env <- tracked_environment(); env$x <- 0
      sr <- stageRunner$new(env, list(function(e) e$x <- 1,
        list(function(e) e$x <- 2, function(e) e$x <- 3)), remember = TRUE)
      sr$run()
      expect_equal(env$x, 3)
      envs <- sr$run('2/1')
      expect_equal(envs$before$x, 1)
      expect_equal(envs$after$x, 2)
    })
  })
})

