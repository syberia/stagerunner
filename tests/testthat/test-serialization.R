context("serialization")

test_that("it can read a serialized stageRunner", {
  sr <- stageRunner$new(new.env(), function(e) e$x <- 1)
  tmp <- tempfile()
  saveRDS(sr, tmp)
  sr2 <- readRDS(tmp)
  sr2$run()
  expect_equal(sr2$context$x, 1)
})
