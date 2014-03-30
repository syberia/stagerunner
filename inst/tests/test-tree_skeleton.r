context('tree skeleton')

tmpClass <- setRefClass('tmpClass')
tmpClass2 <- setRefClass('tmpClass2', methods = list(blah = force))

test_that('it errors when not given a reference class object on initialization', {
  expect_error(treeSkeleton$new(1, '', ''), 'should be from class “refClass” or a subclass')
})

test_that('it errors when not given methods of a reference class objects for the callers', {
  expect_error(treeSkeleton$new(tmpClass$new(), '', ''), 'methods\\(\\)) is not TRUE')
  out <- tryCatch(treeSkeleton$new(tmpClass$new(), 'blah', 'blah'), error = function(.) NULL)
  expect_false(identical(out, NULL))
  removeClass('tmpClass2')
})
