context('treeSkeleton')

test_that('it errors when not given methods of a reference class object for the callers', {
  sr <- stageRunner$new(new.env(), list())
  expect_error(treeSkeleton$new(sr, '', ''), 'methods\\(\\)) is not TRUE')
})

test_that('it does not error when given different child and parent callers on an S3 object', {
  testlist <- list()
  tmp <- list(list(1), list(2))
  attr(tmp[[1]], 'childs') <- list(); attr(tmp[[1]], 'child_index') <- 1
  attr(tmp[[2]], 'childs') <- list(); attr(tmp[[2]], 'child_index') <- 2
  attr(testlist, 'childs') <- tmp; attr(testlist, 'par') <- NULL
  out <- tryCatch(treeSkeleton$new(testlist, 'childs', 'par'), error = function(.) NULL)
  expect_false(identical(out, NULL))
})

test_that('it does not error when given different child and parent callers on a reference class object', {
  local({
    #testobj <- suppressWarnings(setRefClass('testclass', methods = list(childs = function() list()))$new())
    #out <- tryCatch(treeSkeleton$new(testobj, 'childs', 'childs'), error = function(.) NULL)
    #expect_false(identical(out, NULL))
  })
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

test_that('it returns NULL if a node has no successor', {
  sr <- stageRunner$new(new.env(),
    list(list(tmp <- function(x) x + 1, function(x) x + 2), force), remember = T)
  ts <- treeSkeleton$new(sr$stages[[2]])
  expect_identical(ts$successor(), NULL)
  sr <- stageRunner$new(new.env(),
    list(force, list(force, force)), remember = T)
  ts <- treeSkeleton$new(sr$stages[[2]]$stages[[2]])
  expect_identical(ts$successor(), NULL)
})

# TODO: Write tests for S3 and S4 treeSkeleton calls

