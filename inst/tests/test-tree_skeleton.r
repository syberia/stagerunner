context('tree skeleton')

test_that('it errors when not given methods of a reference class object for the callers', {
  sr <- stageRunner$new(new.env(),list())
  expect_error(treeSkeleton$new(sr, '', ''), 'methods\\(\\)) is not TRUE')
})

test_that('it does not error when given methods of a reference class object for the callers', {
  sr <- stageRunner$new(new.env(),list())
  out <- tryCatch(treeSkeleton$new(sr, 'run', 'run'), error = function(.) NULL)
  expect_false(identical(out, NULL))
})

test_that('it finds a successor of a simple stageRunner object correctly', {
  sr <- stageRunner$new(new.env(), list(force, list(force, force)), remember = T)
  ts <- treeSkeleton$new(sr$stages[[2]])
  expect_equal(ts$.parent_index(), 2)
})

# TODO: Write tests for S3 and S4 treeSkeleton calls
