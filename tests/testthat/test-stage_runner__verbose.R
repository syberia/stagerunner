context("verbose runs")

test_that("it can run verbosely for a simple stagerunner", {
  sr <- stageRunner$new(new.env(),
    list(a = force, b = list(c = force, d = force)))
  expect_output(sr$run(verbose = TRUE), "Running 1.*a.*\\.\\.\\.")
  expect_output(sr$run(verbose = TRUE), "Beginning 2.*b.*stage\\.\\.\\.")
  expect_output(sr$run(verbose = TRUE), "Running 1.*c.*\\.\\.\\.")
  expect_output(sr$run(verbose = TRUE), "Ending 2.*b.*stage\\.\\.\\.")
})
