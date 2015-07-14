context("serialization")

test_that("it can read a serialized stageRunner", {
  sr <- stageRunner$new(new.env(), function(e) e$x <- 1)
  tmp <- tempfile()
  saveRDS(sr, tmp)
  sr2 <- readRDS(tmp)
  sr2$run()
  expect_equal(sr2$context$x, 1)
})

test_that("it can resume a serialized stageRunner", {
  sr <- stageRunner$new(new.env(), remember = TRUE,
    list(function(e) e$x <- 1, function(e) e$y <- 1))
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  saveRDS(sr, tmp1)
  sr$run(1)
  saveRDS(sr, tmp2)
  sr2 <- readRDS(tmp1)
  sr3 <- readRDS(tmp2)
  expect_error(sr2$run(2), "Cannot run this stage yet")
  expect_equal(sr3$context$x, 1)
  sr3$run(2)
  expect_equal(sr3$context$y, 1)
})


