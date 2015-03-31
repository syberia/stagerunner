context('copy_env')

# A helper function for checking whether copy_env is working properly.
# See also: http://stackoverflow.com/questions/22675046/transforming-a-nested-environment-into-a-nested-list/22675108#22675108
nested_env_list <- function(env) {
  out <- as.list(env)
  lapply(out, function(x) if (is.environment(x)) nested_env_list(x) else x)
}

test_that('it should copy an empty environment', {
  tmp <- new.env(); tmp2 <- new.env()
  copy_env(tmp2, tmp)
  expect_equal(length(tmp2), 0)
})

test_that('it should copy a flat environment', {
  tmp <- new.env(); tmp$x <- 1; tmp$y <- iris; tmp$z <- function(x) x + 1
  tmp2 <- new.env()
  copy_env(tmp2, tmp)
  expect_identical(nested_env_list(tmp2), nested_env_list(tmp))
})

test_that('it should copy a nested environment', {
  tmp <- new.env(); tmp$x <- 1; tmp$y <- iris; tmp$z <- function(x) x + 1
  tmp$w <- new.env(); tmp$w$x <- 5; tmp$w$y <- mtcars; tmp$w$z <- function(x) x + 2
  tmp2 <- new.env()
  copy_env(tmp2, tmp)
  expect_identical(nested_env_list(tmp2), nested_env_list(tmp))
})

