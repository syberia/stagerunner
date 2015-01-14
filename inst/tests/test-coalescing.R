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
})

describe('with tracked_environments', {
  library(objectdiff)
})
