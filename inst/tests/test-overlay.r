context('stageRunner overlaying')

test_that('it can overlay a simple example correctly', {
  sr1 <- stageRunner$new(cx <- new.env(), list(a = function(x) x$x <- 1, b = function(y) y$x <- 3))
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
  # This is a little tricky to test. We don't want to be reaching into the
  # internals, but we have no choice here.
  sr1 <- stageRunner$new(cx <- new.env(), list(a = function(x) x$x <- 1, b = function(y) y$x <- 3))
  sr2 <- stageRunner$new(cx, list(a = function(y) y$x <- 2))
  sr1$overlay(sr2, 'two')
  expect_identical(names(sr1$stages[[1]]$callable$stages), c('', 'two'))
})

test_that('it gives an error if keys do not match overlay', {
  sr1 <- stageRunner$new(cx <- new.env(), list(a = identity))
  sr2 <- stageRunner$new(cx, list(a2 = identity))
  expect_error(sr1$overlay(sr2), 'keys do not match')
})

test_that('it overlays by index if no names are present', {
  sr1 <- stageRunner$new(cx <- new.env(), list(function(x) x$x <- 1, function(y) y$x <- 3))
  sr2 <- stageRunner$new(cx, list(function(y) y$x <- 2))
  sr1$overlay(sr2)
  sr1$run(1)
  expect_identical(cx$x, 2)
})

test_that('it overlays by index if no names are present with a NULL overlay', {
  sr1 <- stageRunner(cx <- new.env(), list(function(x) x$x <- 1, function(y) y$x <- 3))
  sr2 <- stageRunner(cx, list(NULL, function(y) y$x <- 2))
  sr1$overlay(sr2)
  sr1$run()
  expect_identical(cx$x, 2)
  # TODO: (RK) Figure out a way to check this without reaching into internals
  expect_is(sr1$stages[[1]]$callable, 'function',
    info = paste0('Since the first stage in the second stageRunner was NULL, ',
                  'the overlayed stageRunnerNode should not created a nested ',
                  'stageRunner as its callable.'))
})

test_that('it correctly uses the flat parameter', {
  sr1 <- stageRunner(cx <- new.env(), list(a = function(x) x$x <- 1))
  sr2 <- stageRunner(cx, list(a = function(x) x$y <- 1))
  sr1$overlay(sr2, label = 'two', flat = FALSE)
  sr3 <- stageRunner(cx, list(a = function(x) x$z <- 1))
  sr1$overlay(sr3, label = 'two', flat = FALSE)
  sr1$run()

  expect_identical(cx$y, 1, info = 'both sr2 and sr3 should have ran, but y is not 1')
  expect_identical(cx$z, 1, info = 'both sr2 and sr3 should have ran, but z is not 1')
})

test_that('it correctly uses the flat parameter', {
  sr1 <- stageRunner(cx <- new.env(), list(a = function(x) x$x <- 1))
  sr2 <- stageRunner(cx, list(a = function(x) x$y <- 1))
  sr1$overlay(sr2, label = 'two', flat = TRUE)
  sr3 <- stageRunner(cx, list(a = function(x) x$z <- 1))
  sr1$overlay(sr3, label = 'two', flat = TRUE)
  sr1$run()
  
  expect_identical(cx$y, NULL, info = 'only sr3 should have ran, but y is not NULL')
  expect_identical(cx$z, 1, info = 'only sr3 should have ran, but z is not 1')
})

