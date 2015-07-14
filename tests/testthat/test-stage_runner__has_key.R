context('stagerunner$has_key')

test_that("it tells when a stagerunner does not have a key", {
  expect_false(stagerunner(new.env(), list("foo" = force))$has_key("bar"))
})

test_that("it tells when a stagerunner has a key", {
  expect_true(stagerunner(new.env(), list("foo" = force))$has_key("foo"))
})

test_that("it tells when a stagerunner has a partial matching key", {
  expect_true(stagerunner(new.env(), list("foo" = list("bar" = force)))$has_key("f/ba"))
})

test_that("it tells when a stagerunner has a partial matching key numerically given", {
  expect_true(stagerunner(new.env(), list("foo" = list("bar" = force, "baz" = force)))$has_key("1/2"))
})

test_that("it tells when a stagerunner has a partial matching key numerically given", {
  expect_false(stagerunner(new.env(), list("foo" = list("bar" = force, "baz" = force)))$has_key("1/3"))
})

