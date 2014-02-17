context("stage runner")

test_that("it runs a simple single stage correctly", {
  context <- new.env()
  context$x <- 1
  sr <- stageRunner$new(context, list(function(cx) cx$x <- 2))
  sr$run()
  expect_equal(2, context$x)
})

test_that("it finds a stage by full key name", {
  context <- new.env()
  context$x <- 1; context$y <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3))
  sr$run('stage_one')
  expect_equal(2, context$x); expect_equal(1, context$y)
})

test_that("it finds a stage by partial key name", {
  context <- new.env()
  context$x <- 1
  context$y <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3))
  sr$run('one')
  expect_equal(2, context$x); expect_equal(1, context$y)
})

test_that("it finds a stage by logical indexing", {
  context <- new.env()
  context$x <- 1; context$y <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3))
  sr$run(c(TRUE, FALSE))
  expect_equal(2, context$x); expect_equal(1, context$y)
})

test_that("it finds a stage by logical indexing", {
  context <- new.env()
  context$x <- 1; context$y <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3))
  sr$run(c(TRUE, FALSE))
  expect_equal(2, context$x); expect_equal(1, context$y)
})

test_that("it finds a stage by numeric indexing", {
  context <- new.env()
  context$x <- 1; context$y <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3))
  sr$run(1)
  expect_equal(2, context$x); expect_equal(1, context$y)
})

test_that("it finds a stage by negative indexing", {
  context <- new.env()
  context$x <- 1; context$y <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3))
  sr$run(-2)
  expect_equal(2, context$x); expect_equal(1, context$y)
})

test_that("it finds two stages by partial match", {
  context <- new.env()
  context$x <- 1; context$y <- 1; context$z <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3,
                                      stage_three = function(cx) cx$z <- 4))
  sr$run(c('one', 'three'))
  # Expect only stages 1 and 3 to have been run
  expect_equal(2, context$x); expect_equal(1, context$y); expect_equal(4, context$z)
})

test_that("disallow running stages out of order", {
  context <- new.env()
  context$x <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$x <- 3))
  expect_warning(sr$run(2:1), "out of order")
  expect_equal(3, context$x,
    info = paste0("Stage two must have run after stage one, even though the ",
                  "run() method was passed 2:1"))
})

