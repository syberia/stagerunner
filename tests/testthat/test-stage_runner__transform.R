context("stagerunner$transform")

test_that("a transform call works as expected", {
  sr <- stageRunner$new(new.env(),
    list(a = function(e) e$x <- 1, list(b = function(e) e$y <- 2,
                                        c = function(e) e$z <- 3)))
  sr$transform(function(fn) {
    force(fn)
    function(...) {
      cat(".")
      fn(...)
    }
  })

  expect_output(sr$run(), "^\\.\\.\\.$")
})


test_that("a transform call works as expected with an overlayed runner", {
  sr <- stageRunner$new(new.env(),
    list(a = function(e) e$x <- 1, list(b = function(e) e$y <- 2,
                                        c = function(e) e$z <- 3)))
  sr2 <- stageRunner$new(new.env(), list(a = function(e) e$x <- 2))
  sr$overlay(sr2)

  sr$transform(function(fn) {
    force(fn)
    function(...) {
      cat(".")
      fn(...)
    }
  })

  expect_output(sr$run(), "^\\.\\.\\.\\.$")
})
