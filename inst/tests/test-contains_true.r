context('contains_true')

test_that('it handles an empty list correctly', {
  expect_false(contains_true(list()))
  expect_false(contains_true(logical(0)))
})

test_that('it detects an atomic logical correctly', {
  expect_true(contains_true(TRUE))
  expect_false(contains_true(FALSE))
})

test_that('it detects a TRUE in a vector of logicals correctly', {
  expect_true(contains_true(c(FALSE, FALSE, TRUE)))
  expect_false(contains_true(c(FALSE, FALSE, FALSE)))
})

test_that('it detects a TRUE in a list of logicals correctly', {
  expect_true(contains_true(list(TRUE)))
  expect_true(contains_true(list(FALSE, list(TRUE, FALSE))))
  expect_true(contains_true(list(FALSE, list(FALSE, FALSE), list(FALSE, list(list(TRUE))))))
})

test_that('it detects no TRUE in a list of logicals correctly', {
  expect_false(contains_true(list(FALSE)))
  expect_false(contains_true(list(FALSE, list(FALSE, FALSE))))
  expect_false(contains_true(list(FALSE, list(FALSE, FALSE), list(FALSE, list(list(FALSE))))))
})

