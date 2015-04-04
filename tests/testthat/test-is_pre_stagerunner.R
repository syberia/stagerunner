context("is_pre_stagerunner")

test_that("Non listy stuff can't be turned into stageRunners", {
  expect_false(is_pre_stagerunner(5))
  expect_false(is_pre_stagerunner(new.env()))
  expect_false(is_pre_stagerunner(list(TRUE, force)))
})

test_that("a function can be turned into a stageRunner", {
  expect_true(is_pre_stagerunner(force))
  expect_true(is_pre_stagerunner(function(x) x$x <- 1))
})

test_that("a stageRunner can be turned into a stageRunner", {
  sr <- stageRunner(new.env(), force)
  expect_true(is_pre_stagerunner(sr))
})

test_that("a stageRunner list can be turned into a stageRunner", {
  sr <- stageRunner(new.env(), force)
  expect_true(is_pre_stagerunner(list(sr, force)))
})

test_that("a stageRunner list with a NULL can be turned into a stageRunner", {
  sr <- stageRunner(new.env(), force)
  expect_true(is_pre_stagerunner(list(sr, NULL, force)))
})

