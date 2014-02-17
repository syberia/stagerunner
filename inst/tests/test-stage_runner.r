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
  sr$run(list('one', 'three'))
  # Expect only stages 1 and 3 to have been run
  expect_equal(2, context$x); expect_equal(1, context$y); expect_equal(4, context$z)
})

test_that("it disallows running stages out of order", {
  context <- new.env()
  context$x <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$x <- 3))
  # TODO: The line below is old from when we didn't have fancy nested lists.
  # See if it still makes sense ...
  # expect_warning(sr$run(2:1), "out of order")
  sr$run(2:1)
  expect_equal(3, context$x,
    info = paste0("Stage two must have run after stage one, even though the ",
                  "run() method was passed 2:1"))
})

# We will be using / for recursive nested stagerunners, so don't allow this for now
test_that("it disallows stage names with a '/' character", {
  context <- new.env()
  expect_error(
    stageRunner$new(context, list('stage / one' = function(cx) cx$x <- 2)),
    "may not have a '\\/' character")
})

test_that("it accepts nested stagerunners", {
  context <- new.env()
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2))
  sr2 <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 3))
  tryattempt <- tryCatch(stageRunner$new(context,
    list(sr, sr2, function(cx) cx$x <- 4)), error = function(...) NULL)
  expect_false(is.null(tryattempt), "stagerunners must be able to take nested stagerunners")
})

test_that("it runs nested stagerunners", {
  context <- new.env()
  context$x <- 0
  sr2 <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2))
  sr3 <- stageRunner$new(context,
    list(stage_one = function(cx) cx$x <- 3, stage_oneb = function(cx) cx$x <- 5 ))
  sr <- stageRunner$new(context,
    list(sr2, sr3, stage_three = function(cx) cx$x <- 4))
  sr$run()
  expect_equal(context$x, 4, "stagerunner must execute nested stages")

  sr <- stageRunner$new(context,
    list(stage_three = function(cx) cx$x <- 4, sr2, sr3))
  sr$run()
  expect_equal(context$x, 5, "stagerunner must execute nested stages")
})

test_that("it allows referencing nested stage keys", {
  context <- new.env(); context$x <- 0; context$y <- 0
  sr2 <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2))
  sr <- stageRunner$new(context, list(sub = sr2, function(cx) cx$y <- 1))
  sr$run('sub/stage_one')
  expect_equal(context$x, 2, "stagerunner must execute nested stages referred to by keyname")
  expect_equal(context$y, 0, "stagerunner must not execute unrun stages")
})






