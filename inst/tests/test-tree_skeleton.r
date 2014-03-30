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

test_that('it finds the index as a child of a simple stageRunner object correctly', {
  sr <- stageRunner$new(new.env(), list(force, list(force, force)), remember = T)
  ts <- treeSkeleton$new(sr$stages[[2]])
  expect_equal(ts$.parent_index(), 2)
})

test_that('it finds the successor of a simple stageRunner object correctly when it is a next leaf', {
  sr <- stageRunner$new(new.env(),
    list(force, list(tmp <- function(x) x + 1, function(x) x + 2)), remember = T)
  ts <- treeSkeleton$new(sr$stages[[1]])
  expect_identical(ts$successor()$object, sr$stages[[2]]$stages[[1]])
})

test_that('it finds the successor of a simple stageRunner object correctly when it is a parent\'s successor', {
  sr <- stageRunner$new(new.env(),
    list(list(tmp <- function(x) x + 1, function(x) x + 2), force), remember = T)
  ts <- treeSkeleton$new(sr$stages[[1]]$stages[[2]])
  expect_identical(ts$successor()$object, sr$stages[[2]])
})

# TODO: Write tests for S3 and S4 treeSkeleton calls
