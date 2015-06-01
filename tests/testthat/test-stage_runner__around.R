library(testthatsomemore)

context('stageRunnerNode$around')

test_that('it throws an error when an invalid $around operation is performed', {
  sn <- stageRunnerNode$new(function() { })
  expect_warning(sn$around(list(function() {} )),
               'Cannot apply stageRunner\\$around')
})

test_that('it correctly wraps a simple example', {
  sn <- stageRunnerNode$new(function(e) cat('2'))
  sn$around(function(e) { cat('1'); yield(); cat('3'); })
  expect_output(sn$run(), '^123$')
})

test_that('it correctly wraps a simple example with run-time arguments', {
  sn <- stageRunnerNode$new(function(e, x) cat(x))
  sn$around(function(e, x) { cat('1'); yield(); cat('3'); })
  expect_output(sn$run(x = 2), '^123$')
})

context('stageRunner$around')

test_that('it does not error when NULL is passed to $around', {
  sr <- stageRunner$new(new.env(), list())
  assert(sr$around(NULL))
})

test_that('it correctly runs an around block on a simple example', {
  sr <- stageRunner$new(new.env(), list(a = function(e) cat('2')))
  sr$around(list(a = function(e) { cat('1'); yield(); cat('3') }))
  expect_output(sr$run(), '^123$')

  sr <- stageRunner$new(new.env(), list(a = function(e) cat('2')))
  sr$around(list(function(e) { cat('1'); yield(); cat('3') }))
  expect_output(sr$run(), '^123$')
})

test_that('it correctly runs an around block on a nested example', {
  sr <- stageRunner$new(new.env(),
    list(a = function(e) cat('2'), b = function(e) cat('4')))
  sr$around(list(a = function(e) { cat('1'); yield(); cat('3') }))
  expect_output(sr$run(), '^1234$')

  sr <- stageRunner$new(new.env(),
    list(a = function(e) cat('2'), b = function(e) cat('4')))
  sr$around(list(a = function(e) { cat('1'); yield(); cat('3') },
                 function(e) { yield(); cat('5'); }))
  expect_output(sr$run(), '^12345$')
})

test_that('it correctly wraps a simple example with run-time arguments', {
  sn <- stageRunner$new(new.env(), list(a = function(e, x) cat(x)))
  sn$around(function(e, x) { cat('1'); yield(); cat('3'); })
  expect_output(sn$run(x = 2), '^123$')
})

test_that('it can nest $around calls, like Rack Middleware!', {
  sr <- stageRunner$new(new.env(), function(e) cat('2'))
  sr$around(function(e) { cat('1'); yield(); cat('3') })
  sr$around(function(e) { cat('0'); yield(); cat('4') })
  expect_output(sr$run(), '^01234$')
})

test_that('it throws a warning when an invalid $around operation is performed', {
  sr <- stageRunner$new(new.env(), list(a = function(e) cat('2')))
  expect_warning(sr$around(list(a = list(b = function(e) {} ))),
                           'Cannot apply stageRunner$around')
})

