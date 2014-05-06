context('stageRunner overlaying')

test_that('it can overlay a simple example correctly', {
  sr1 <- stageRunner$new(cx <- new.env(), list(a = function(x) x$x <- 1, b = function(y) x$x <- 3))
  sr2 <- stageRunner$new(cx, list(a = function(y) y$x <- 2))
  sr1$overlay(sr2)
  sr1$run(1)
  expect_identical(cx$x, 2)

  # Check that reverse order works as well.
  sr1 <- stageRunner$new(cx <- new.env(), list(a = function(x) x$x <- 1, b = function(x) x$x <- 3))
  sr2 <- stageRunner$new(cx, list(a = function(y) y$x <- 2, b = function(x) x$x <- 3))
  sr2$overlay(sr1)
  sr1$run(1)
  expect_identical(cx$x, 1)
})

test_that('it can give labels to overlays', {
  
})
