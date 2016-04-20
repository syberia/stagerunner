library(testthatsomemore)
context('stageRunner coalescing')

describe("invalid inputs", {
  test_that("it fails to coalesce a vanilla runner with a tracked runner", {
    sr1 <- stageRunner$new(new.env(), force, remember = TRUE)
    sr2 <- stageRunner$new(objectdiff::tracked_environment(new.env()), force, remember = TRUE)
    expect_error(sr1$coalesce(sr2), "Cannot coalesce")
    expect_error(sr2$coalesce(sr1), "Cannot coalesce")
  })
})

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

  test_that("it coalesces when a stage is removed further in the chain", {
    sr1 <- stageRunner$new(new.env(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 2, c = function(e) e$z <- 3))
    sr2 <- stageRunner$new(new.env(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 4, d = function(e) e$z <- 5))
    sr1$run(1)
    sr2$coalesce(sr1)
    assert(sr2$run(2))
    expect_identical(sr2$context$y, 4)
  })

  test_that("it coalesces for substages", {
    sr1 <- stageRunner$new(new.env(), remember = TRUE,
      list(a = function(e) e$x <- 1,
           b = list(b1 = function(e) e$y <- 2, b2 = function(e) e$z <- 3, b3 = function(e) e$w <- 4)))
    sr2 <- stageRunner$new(new.env(), remember = TRUE,
      list(a = function(e) e$x <- 1,
           b = list(b1 = function(e) e$y <- 2, b2 = function(e) e$z <- 5, b3 = function(e) e$w <- 6)))
    sr1$run('a', 'b/b2')
    sr2$coalesce(sr1)
    assert(sr2$run('b/b2', 'b'))
    expect_identical(sr2$context$z, 5)
    expect_identical(sr2$context$w, 6)
  })

  test_that("it does not coalesce a stage when run with remember_flag = FALSE", {
    sr1 <- stageRunner$new(new.env(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 2,
           c = function(e) e$z <- 3))
    sr2 <- stageRunner$new(new.env(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 4,
           c = function(e) e$z <- 5))
    sr1$run(1)
    sr1$run(2, remember_flag = FALSE)
    sr2$coalesce(sr1)
    expect_error(sr2$run(3), "Cannot run this stage")
  })
})

describe('with tracked_environments', {
  library(objectdiff)

  test_that("it can coalesce a trivial example", {
    sr1 <- stageRunner$new(tracked_environment(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 2,
           c = function(e) e$z <- 3))
    sr2 <- stageRunner$new(tracked_environment(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 4,
           c = function(e) e$z <- 5))
    sr1$run(1)
    sr2$coalesce(sr1)
    assert(sr2$run(2))
    expect_identical(sr2$context$y, 4)
  })

  test_that("it does not coalesce when no names overlap", {
    sr1 <- stageRunner$new(tracked_environment(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 2))
    sr2 <- stageRunner$new(tracked_environment(), remember = TRUE,
      list(c = function(e) e$x <- 1, d = function(e) e$y <- 4))
    sr1$run()
    sr2$coalesce(sr1)
    expect_error(sr2$run(2), "some previous stages have not been executed")
  })

  test_that("it can coalesce when an immediate successor stage is renamed", {
    sr1 <- stageRunner$new(tracked_environment(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 2))
    sr2 <- stageRunner$new(tracked_environment(), remember = TRUE,
      list(a = function(e) e$x <- 1, c = function(e) e$y <- 4))
    sr1$run(1)
    sr2$coalesce(sr1)
    sr2$run(2)
    expect_identical(sr2$context$x, 1)
    expect_identical(sr2$context$y, 4)
  })

  test_that("it cannot coalesce when a later successor stage is renamed", {
    sr1 <- stageRunner$new(tracked_environment(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 2, c = function(e) e$z <- 3))
    sr2 <- stageRunner$new(tracked_environment(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 4, d = function(e) e$w <- 5))
    sr1$run(1)
    sr2$coalesce(sr1)
    expect_error(sr2$run(3), "some previous stages have not been executed")
  })

  test_that("it coalesces when a stage is removed further in the chain", {
    sr1 <- stageRunner$new(tracked_environment(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 2, c = function(e) e$z <- 3))
    sr2 <- stageRunner$new(tracked_environment(), remember = TRUE,
      list(a = function(e) e$x <- 1, b = function(e) e$y <- 4, d = function(e) e$z <- 5))
    sr1$run(1)
    sr2$coalesce(sr1)
    assert(sr2$run(2))
    expect_identical(sr2$context$y, 4)
  })

  test_that("it coalesces for substages", {
    sr1 <- stageRunner$new(tracked_environment(), remember = TRUE,
      list(a = function(e) e$x <- 1,
           b = list(b1 = function(e) e$y <- 2, b2 = function(e) e$z <- 3, b3 = function(e) e$w <- 4)))
    sr2 <- stageRunner$new(tracked_environment(), remember = TRUE,
      list(a = function(e) e$x <- 1,
           b = list(b1 = function(e) e$y <- 2, b2 = function(e) e$z <- 5, b3 = function(e) e$w <- 6)))
    sr1$run('a', 'b/b2')
    sr2$coalesce(sr1)
    assert(sr2$run('b/b2', 'b'))
    expect_identical(sr2$context$z, 5)
    expect_identical(sr2$context$w, 6)
  })

  test_that("coalescing works in conjunction with nested terminal nodes", {
    sr <- stageRunner$new(new.env(), list(function(e) e$x <- 1, function(e) e$x <- 2), remember = TRUE)
    sr$around(stageRunner$new(new.env(), list(function(e) { e$x <- 0; yield(); e$x <- 3 })))
    sr2 <- stageRunner$new(new.env(), list(function(e) e$x <- 4, function(e) e$x <- 5))
    sr$run(1)
    sr2$coalesce(sr)
    sr2$run(2)
    expect_identical(sr2$context$x, 5)
  })

  test_that("coalescing works in conjunction with nested terminal nodes on the coalescee", {
    sr <- stageRunner$new(new.env(), list(function(e) e$x <- 1, function(e) e$x <- 2), remember = TRUE)
    sr$around(stageRunner$new(new.env(), list(function(e) { e$x <- 0; yield(); e$x <- 3 })))
    sr2 <- stageRunner$new(new.env(), list(function(e) e$x <- 4, function(e) e$x <- 5), remember = TRUE)
    sr2$run(1)
    sr$coalesce(sr2)
    # TODO: (RK) Fix this test.
    # sr$run(2)
    # expect_identical(sr$context$x, 5)
  })

  test_that("it coalesces during name changes", {
    stages1 <- setNames(replicate(15, list(a = function(e) e$x <- 1)), letters[1:15])
    stages1$p <- function(e) e$y <- 2
    sr1 <- stageRunner$new(tracked_environment(), remember = TRUE, list(foo = stages1))
    stages2 <- stages1
    names(stages2)[16] <- 'q'
    sr2 <- stageRunner$new(tracked_environment(), remember = TRUE, list(foo = stages2))
    sr1$run(to = "1/15")
    sr2$coalesce(sr1)
    sr2$run("1/16")
    expect_identical(sr2$context$y, 2)
  })
})

test_that("it can coalesce a deeper stagerunner", {
  test_that("it can coalesce deep runners with vanilla environments", {
    runner1 <- function() {
      stageRunner$new(new.env(), list(foo = list(bar = list(baz = function(e) e$x <- 1, qux = function(e) e$y <- 1)),
                                      second = list(sub1 = function(e) e$z <- 1, sub2 = function(e) e$w <- 1)),
                      remember = TRUE)
    }
    runner2 <- function() {
      stageRunner$new(new.env(), list(foo = list(bar = list(baz = function(e) e$x <- 2, qux = function(e) e$y <- 2)),
                      second = list(sub1 = function(e) e$z <- 2, sub2 = function(e) e$w <- 2)),
                      remember = TRUE)
    }

    r1 <- runner1(); r1$run(to = "1/1/1")
    r2 <- runner2(); r2$coalesce(r1)
    r2$run("1/1/2")
    expect_true(setequal(as.list(r2$context), list(x = 1, y = 2)))

    r1 <- runner1(); r1$run(to = 1)
    r2 <- runner2(); r2$coalesce(r1)
    r2$run("2/1")
    expect_true(setequal(as.list(r2$context), list(x = 1, y = 1, z = 2)))

    r1 <- runner1(); r1$run(to = "2/1")
    r2 <- runner2(); r2$coalesce(r1)
    r2$run("2/2")
    expect_true(setequal(as.list(r2$context), list(x = 1, y = 1, z = 2, w = 2)))
  })

  test_that("it can coalesce deep runners with tracked environments", {
    runner1 <- function() {
      stageRunner$new(objectdiff::tracked_environment(),
                      list(foo = list(bar = list(baz = function(e) e$x <- 1, qux = function(e) e$y <- 1)),
                           second = list(sub1 = function(e) e$z <- 1, sub2 = function(e) e$w <- 1)),
                      remember = TRUE)
    }
    runner2 <- function() {
      stageRunner$new(objectdiff::tracked_environment(),
                      list(foo = list(bar = list(baz = function(e) e$x <- 2, qux = function(e) e$y <- 2)),
                           second = list(sub1 = function(e) e$z <- 2, sub2 = function(e) e$w <- 2)),
                      remember = TRUE)
    }

    r1 <- runner1(); r1$run(to = "1/1/1")
    r2 <- runner2(); r2$coalesce(r1)
    r2$run("1/1/2")
    expect_true(setequal(as.list(r2$context), list(x = 1, y = 2)))

    r1 <- runner1(); r1$run(to = 1)
    r2 <- runner2(); r2$coalesce(r1)
    r2$run("2/1")
    expect_true(setequal(as.list(r2$context), list(x = 1, y = 1, z = 2)))

    r1 <- runner1(); r1$run(to = "2/1")
    r2 <- runner2(); r2$coalesce(r1)
    r2$run("2/2")
    expect_true(setequal(as.list(r2$context), list(x = 1, y = 1, z = 2, w = 2)))
  })
})

