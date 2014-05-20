context('stageRunnerNode')

test_that('it initializes a simple node correctly', {
  node <- stageRunnerNode(function(context) context$x <- 1, context <- new.env())
  expect_is(node, 'stageRunnerNode')
  node$run()
  expect_equal(context$x, 1)
})

