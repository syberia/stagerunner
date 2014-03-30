context('stageRunnerNode')

test_that('it initializes a simple node correctly', {
  node <- tryCatch(stageRunnerNode(function(x) x + 1, 1), error = function(x) NULL)
  expect_false(identical(node, NULL))
  expect_is(node, 'stageRunnerNode')
  expect_equal(node$fn(1), 2)
})
