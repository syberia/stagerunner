context('caching on a stageRunner object')

# A helper function for checking whether copy_env is working properly.
# See also: http://stackoverflow.com/questions/22675046/transforming-a-nested-environment-into-a-nested-list/22675108#22675108
nested_env_list <- function(env) {
  out <- as.list(env)
  lapply(out, function(x) if (is.environment(x)) nested_env_list(x) else x)
}

test_that('the first cached environment is simply the initial context', {
  tmp <- new.env(); tmp$x <- 1; tmp$y <- function(z) z + 1; tmp$w <- list(1, iris)
  sr <- stageRunner$new(tmp, list(force), remember = TRUE)
  expect_identical(nested_env_list(sr$stages[[1]]$cached_env), nested_env_list(tmp))
})

test_that('the first cached environment is simply the initial context in a first nested stage', {
  tmp <- new.env(); tmp$x <- 1; tmp$y <- function(z) z + 1; tmp$w <- list(1, iris)
  sr <- stageRunner$new(tmp, list(list(list(force))), remember = TRUE)
  expect_identical(nested_env_list(sr$stages[[1]]$stages[[1]]$stages[[1]]$cached_env),
                   nested_env_list(tmp))
})

test_that('there are no cached environments on non-first stages before anything is run', {
  sr <- stageRunner$new(new.env(), list(force, force, force), remember = TRUE)
  expect_identical(lapply(sr$stages[-1], `$`, "cached_env"), list(NULL, NULL))
})

test_that('running the first two stages updates the cache for the second stage and third stage', {
  sr <- stageRunner$new(new.env(), list(function(env) env$x <- 1, force, force), remember = TRUE)
  sr$run(c(1,2))
  expect_identical(nested_env_list(sr$stages[[2]]$cached_env), list(x = 1))
  expect_identical(nested_env_list(sr$stages[[3]]$cached_env), list(x = 1))
})

test_that('running the first stage does not update the cache for the fourth stage', {
  sr <- stageRunner$new(new.env(), list(function(env) env$x <- 1, force, force, force), remember = TRUE)
  sr$run(c(1,2))
  expect_identical(sr$stages[[4]]$cached_env, NULL)
})

test_that('running the first nested stage updates the cache for the second non-nested stage', {
  sr <- stageRunner$new(new.env(), list(list(function(env) env$x <- 1, force), force), remember = TRUE)
  expect_identical(sr$stages[[2]]$cached_env, NULL)
  sr$run(1)
  expect_identical(nested_env_list(sr$stages[[2]]$cached_env), list(x = 1))
})

test_that('running the first nested stage updates the cache for the second nested stage', {
  sr <- stageRunner$new(new.env(), list(list(function(env) env$x <- 1, force), list(function(env) env$y <- 1, force)), remember = TRUE)
  expect_identical(sr$stages[[2]]$stages[[1]]$cached_env, NULL)
  sr$run(1)
  expect_identical(nested_env_list(sr$stages[[1]]$stages[[2]]$cached_env), list(x = 1))
})

test_that('we cannot run stages out of order due to caching issues', {
  sr <- stageRunner$new(new.env(), list(list(function(env) env$x <- 1, force), list(function(env) env$y <- 1, force)), remember = TRUE)
  sr$run(1)
  expect_error(sr$run('2/2'), 'Cannot run this stage yet because some previous stages have not been executed')
  sr$run('2/1')
  sr$run('2/2') # it should be runnable now
})

test_that('parents and children get set in a stageRunner tree with caching', {
  sr <- stageRunner$new(new.env(), list(force, list(force, force)), remember = TRUE)
  expect_equal(length(sr$children()), 2)
  expect_equal(length(sr$children()[[2]]$children()), 2)
})

test_that('the environment gets restored from cache upon second execution', {
  env <- new.env(); env$x <- 0
  sr <- stageRunner$new(env, list(function(e) e$x <- 1, list(function(e) e$x <- 2, function(e) e$x <- 3)), remember = TRUE)
  sr$run()
  expect_equal(env$x, 3)
  envs <- sr$run('2/1')
  expect_equal(envs$before$x, 1)
  expect_equal(envs$after$x, 2)
})

