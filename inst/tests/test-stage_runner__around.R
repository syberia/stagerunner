context('stageRunner$around')
library(testthatsomemore)

test_that('it does not error when NULL is passed to $around', {
  sr <- stageRunner$new(new.env(), list())
  assert(sr$around(NULL))
})

test_that('it correctly runs an around block on a simple example', {
  sr <- stageRunner$new(new.env(), list(a = function(e) cat('2')))
  sr$around(list(a = function(e) { cat('1'); yield(); cat('3') }))
  expect_output(sr$run(), '123')

  sr <- stageRunner$new(new.env(), list(a = function(e) cat('2')))
  sr$around(list(function(e) { cat('1'); yield(); cat('3') }))
  expect_output(sr$run(), '123')
})

