context("stageRunner")

example1 <- function() {
  eval.parent(substitute({
    context <- new.env()
    context$x <- 0
    sr2 <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2))
    sr3 <- stageRunner$new(context,
      list(stage_one = function(cx) cx$x <- 3, stage_oneb = function(cx) cx$x <- 5 ))
    sr <- stageRunner$new(context,
      list(sr2, sr3, stage_three = function(cx) cx$x <- 4))
  }))
}

test_that("it runs a simple single stage correctly", {
  context <- new.env()
  context$x <- 1
  sr <- stageRunner$new(context, list(function(cx) cx$x <- 2))
  sr$run()
  expect_equal(scale = 1, tolerance = 0.001, 2, context$x) 
})

test_that("it accepts functions (not lists) as stages", {
  context <- new.env()
  context$x <- 1
  sr <- stageRunner$new(context, function(cx) cx$x <- 2)
  sr$run()
  expect_equal(scale = 1, tolerance = 0.001, 2, context$x)
})

test_that("it runs a simple multi-step stages correctly", {
  context <- new.env()
  context$x <- 1; context$y <- 1; context$z <- 1; context$w <- 1
  sr <- stageRunner$new(context,
    list(function(cx) cx$x <- 2,
         dos = stageRunner$new(context,
           list(sub1 = function(cx) cx$y <- 3, sub2 = function(cx) cx$z <- 4)),
         function(cx) cx$w <- 5
    )
  )
  sr$run()
  expect_equal(scale = 1, tolerance = 0.001, 2, context$x)
  expect_equal(scale = 1, tolerance = 0.001, 3, context$y)
  expect_equal(scale = 1, tolerance = 0.001, 4, context$z)
  expect_equal(scale = 1, tolerance = 0.001, 5, context$w)
})

test_that("it finds a stage by full key name", {
  context <- new.env()
  context$x <- 1; context$y <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3))
  sr$run('stage_one')
  expect_equal(scale = 1, tolerance = 0.001, 2, context$x); expect_equal(scale = 1, tolerance = 0.001, 1, context$y)
})

test_that("it finds a non-first stage by full key name", {
  context <- new.env()
  context$x <- 1; context$y <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3))
  sr$run('stage_two')
  expect_equal(scale = 1, tolerance = 0.001, 1, context$x); expect_equal(scale = 1, tolerance = 0.001, 3, context$y)

})

test_that("it finds a stage by partial key name", {
  context <- new.env()
  context$x <- 1
  context$y <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3))
  sr$run('one')
  expect_equal(scale = 1, tolerance = 0.001, 2, context$x); expect_equal(scale = 1, tolerance = 0.001, 1, context$y)
})

test_that("it finds a stage by logical indexing", {
  context <- new.env()
  context$x <- 1; context$y <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3))
  sr$run(c(TRUE, FALSE))
  expect_equal(scale = 1, tolerance = 0.001, 2, context$x); expect_equal(scale = 1, tolerance = 0.001, 1, context$y)
})

test_that("it finds a stage by logical indexing", {
  context <- new.env()
  context$x <- 1; context$y <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3))
  sr$run(c(TRUE, FALSE))
  expect_equal(scale = 1, tolerance = 0.001, 2, context$x); expect_equal(scale = 1, tolerance = 0.001, 1, context$y)
})

test_that("it finds a stage by numeric indexing", {
  context <- new.env()
  context$x <- 1; context$y <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3))
  sr$run(1)
  expect_equal(scale = 1, tolerance = 0.001, 2, context$x); expect_equal(scale = 1, tolerance = 0.001, 1, context$y)
})

test_that("it finds a non-first stage by numeric indexing", {
  context <- new.env()
  context$x <- 1; context$y <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3))
  sr$run(2)
  expect_equal(scale = 1, tolerance = 0.001, 1, context$x); expect_equal(scale = 1, tolerance = 0.001, 3, context$y)
})


test_that("it finds a stage by negative indexing", {
  context <- new.env()
  context$x <- 1; context$y <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3))
  sr$run(-2)
  expect_equal(scale = 1, tolerance = 0.001, 2, context$x); expect_equal(scale = 1, tolerance = 0.001, 1, context$y)
})

test_that("it finds two stages by partial match", {
  context <- new.env()
  context$x <- 1; context$y <- 1; context$z <- 1
  sr <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2,
                                      stage_two = function(cx) cx$y <- 3,
                                      stage_three = function(cx) cx$z <- 4))
  sr$run(list('one', 'three'))
  # Expect only stages 1 and 3 to have been run
  expect_equal(scale = 1, tolerance = 0.001, 2, context$x); expect_equal(scale = 1, tolerance = 0.001, 1, context$y); expect_equal(scale = 1, tolerance = 0.001, 4, context$z)
})

test_that("it finds two stages by partial match, one a nested reference", {
  context <- new.env()
  context$x <- 1; context$y <- 1; context$z <- 1; context$w <- 1; context$r <- 1
  sr2 <- stageRunner$new(context, list(substage_one = function(cx) cx$x <- 2,
                                       substage_two = function(cx) cx$w <- 0,
                                       substage_three = function(cx) cx$r <- 0))
  sr <- stageRunner$new(context, list(stage_one = sr2,
                                      stage_two = function(cx) cx$y <- 3,
                                      stage_three = function(cx) cx$z <- 4))
  sr$run(list('one/one', 'three', 'one/three'))
  # Expect only stages 1 and 3 to have been run
  expect_equal(scale = 1, tolerance = 0.001, 2, context$x, info = "x differs")
  expect_equal(scale = 1, tolerance = 0.001, 1, context$y, info = "y differs")
  expect_equal(scale = 1, tolerance = 0.001, 4, context$z, info = "z differs")
  expect_equal(scale = 1, tolerance = 0.001, 1, context$w, info = "w differs")
  expect_equal(scale = 1, tolerance = 0.001, 0, context$r, info = "r differs")
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
  expect_equal(scale = 1, tolerance = 0.001, 3, context$x,
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
  example1()
  sr$run()
  expect_equal(scale = 1, tolerance = 0.001, context$x, 4, "stagerunner must execute nested stages")

  sr <- stageRunner$new(context,
    list(stage_three = function(cx) cx$x <- 4, sr2, sr3))
  sr$run()
  expect_equal(scale = 1, tolerance = 0.001, context$x, 5, "stagerunner must execute nested stages")
})

test_that("it allows referencing nested stage keys", {
  context <- new.env(); context$x <- 0; context$y <- 0
  sr2 <- stageRunner$new(context, list(stage_one = function(cx) cx$x <- 2))
  sr <- stageRunner$new(context, list(sub = sr2, function(cx) cx$y <- 1))
  sr$run('sub/stage_one')
  expect_equal(scale = 1, tolerance = 0.001, context$x, 2, "stagerunner must execute nested stages referred to by keyname")
  expect_equal(scale = 1, tolerance = 0.001, context$y, 0, "stagerunner must not execute unrun stages")
})

test_that("it can detect mixed numeric-character key collisions", {
  example1()
  expect_that(sr$run('1/one', to = '2/one'), throws_error("Multiple stages"))
})

test_that("it correctly references a numeric stage within a character stage", {
  example1()
  out <- tryCatch(sr$run('2/oneb'), error = function(err) 'error')
  expect_false(out == 'error',
    info = 'stagerunner should be able to reference a numeric stage within a character stage')
})

test_that("it throws an error when an invalid stage gets passed", {
  expect_that(stageRunner$new(new.env(), list(1)),
              throws_error('legal_types.*not TRUE'))
})

test_that("it correctly parses a nested list of functions into nested stagerunners", {
  f1 <- function(){1}; f2 <- function(){2}; f3 <- function(){3}
  sr <- stageRunner$new(new.env(), list(list(f1, c = f2), d = f3))
  expect_is(sr$stages[[1]], 'stageRunner')
  expect_identical(body(sr$stages[[1]]$stages[[1]]$callable), body(f1))
  expect_identical(body(sr$stages[[1]]$stages[[2]]$callable), body(f2))
  expect_identical(names(sr$stages[[1]]$stages)[2], 'c')
  expect_identical(names(sr$stages)[2], 'd')
})

test_that("it should be able to run a nested stage", {
  sr <- stageRunner$new(new.env(), list(list(function(env) env$x <- 1, force), list(function(env) env$y <- 1, force)))
  expect_false(is.null(tryCatch(sr$run('2/1'), error = function(.) NULL)))
})

test_that("it correctly uses the to parameter", {
  example1()
  names(sr$stages[[2]]$stages) <- c('stage_onea', 'stage_oneb')
  sr$run('1/one', to = '2/onea')
  expect_equal(scale = 1, tolerance = 0.001, context$x, 3,
    info = "this stagerunner should execute only stages 1/one and 2/one")
})

test_that("it correctly goes to the end of a section", {
  tmp <- new.env()
  sr <- stageRunner$new(tmp,
    list(a = list(b = force, c = function(e)e$z <- 1,
                  d = function(e) e$x <- 1, e = function(e) e$y <- 1)))
  sr$run('a/d', 'a')
  expect_false('z' %in% ls(tmp), 'must run just a/d and a/e')
  expect_true(all(c('y', 'x') %in% ls(tmp)), 'must run just a/d and a/e')
})

test_that("it correctly uses the to parameter in a more complicated example", {
  context <- new.env()
  fn <- function(x) {
    name <- deparse(substitute(x))
    eval(bquote(function(cx) cx[[.(name)]] <- 2 ))
  }

  sr <- stageRunner$new(context, list(
    one = list(a = fn(a), fn(z), b = fn(b)), fn(c),
    two = list(fn(z), list(list(d = fn(d)))),
    three = list(e = fn(e), f = fn(f)),
    fn(g)))
  test_exprs <- list(substitute(sr$run('one/b', to = 'three/e')),
                  substitute(sr$run('three/e', to = 'one/b')))
  lapply(seq_along(test_exprs), function(ix) { 
      with(context, { a <- 1; b <- 1; c <- 1; d <- 1; e <- 1; f <- 1; g <- 1 })
      eval(test_exprs[[ix]])
      expect_equal(scale = 1, tolerance = 0.001, 
        list(z = 2, a = 1, b = 2, c = 2, d = 2, e = 2, f = 1, g = 1), as.list(context),
        info = paste0("this stagerunner should correctly execute all the stages ",
                      "between b and e above, namely b,c,d,e, when running: ",
                      paste0(deparse(test_exprs[[ix]]), collapse = "\n")))
    })
})

### next_stage method
test_that('it can figure out the next stage in a non-caching stageRunner', {
  # TODO: (RK) Maybe it should just track stage execution without necessarily
  # tracking full environments.
  sr <- stageRunner(new.env(), list(force, force))
  expect_identical(sr$next_stage(), '1')
  sr$run(1)
  expect_identical(sr$next_stage(), '2')
  sr$run(2)
  expect_identical(sr$next_stage(), FALSE)
})

test_that('it correctly figures out the next stage in a fresh stageRunner', {
  sr <- stageRunner(new.env(), list(force, force), remember = TRUE)
  expect_identical(sr$next_stage(), '1')
})

test_that('it correctly figures out the next stage in a stageRunner with one executed stage', {
  sr <- stageRunner(new.env(), list(force, force), remember = TRUE)
  sr$run(1)
  expect_identical(sr$next_stage(), '2')
})

test_that('it correctly figures out the next stage in a stageRunner with all executed stages', {
  sr <- stageRunner(new.env(), list(force, force), remember = TRUE)
  sr$run()
  expect_identical(sr$next_stage(), FALSE)
})

test_that('it correctly figures out the next stage in a stageRunner with nested executed stages', {
  sr <- stageRunner(new.env(), list(force, list(force, force)))
  sr$run(to = '2/1')
  expect_identical(sr$next_stage(), '2/2',
    info = 'this stageRunner should not execute the last stage')
  sr$run('2/2')
  expect_identical(sr$next_stage(), FALSE,
    info = 'this stageRunner should claim to be completely done')
})

test_that('it correctly figures out the next stage in a stageRunner with deeply nested executed stages', {
  sr <- stageRunner(new.env(),
    list(a = force, b = list(c = list(force, d = list(force, force, force))), force))
                                # this stage should be marked as next ^

  sr$run(to = 'b/c/d/2')
  expect_identical(sr$next_stage(), '2/1/2/3')
})


