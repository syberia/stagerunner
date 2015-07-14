context('stagerunner$has_key')

test_that("it tells when a stagerunner does no have a key", {
  expect_false(stagerunner(new.env(), list("foo" = force))$has_key("bar"))
})
