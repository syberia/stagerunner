library(testthatsomemore)
context('stageRunner coalescing')

describe('with regular environments', {

  test_that("it can coalesce a trivial example", {
    sr1 <- stageRunner$new(new.env(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 2,
           c = function(e) e$z <- 3))
    sr2 <- stageRunner$new(new.env(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 4,
           c = function(e) e$z <- 5))
    sr1$run(1)
    sr2$coalesce(sr1)
    assert(sr2$run(2))
    expect_identical(sr2$context$y, 4)
  })

  test_that("it does not coalesce when no names overlap", {
    sr1 <- stageRunner$new(new.env(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 2))
    sr2 <- stageRunner$new(new.env(), remember = TRUE,
      list(c = function(e) e$x <- 1, d = function(e) e$y <- 4))
    sr1$run()
    sr2$coalesce(sr1)
    expect_error(sr2$run(2), "some previous stages have not been executed")
  })

  test_that("it cannot coalesce when a stage is renamed", {
    sr1 <- stageRunner$new(new.env(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 2))
    sr2 <- stageRunner$new(new.env(), remember = TRUE,
      list(a = function(e) e$x <- 1, c = function(e) e$y <- 4))
    sr1$run(1)
    sr2$coalesce(sr1)
    expect_error(sr2$run(2), "some previous stages have not been executed")
  })
})

describe('with tracked_environments', {
  library(objectdiff)
})
