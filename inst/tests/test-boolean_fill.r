context('boolean_fill')

test_that('it correctly errors on an empty list, or one missing TRUEs', {
  expect_error(boolean_fill(logical(0)))
  expect_error(boolean_fill(logical(0), forward = FALSE))
  expect_error(boolean_fill(list()))
  expect_error(boolean_fill(list(), forward = FALSE))
  expect_error(boolean_fill(list(FALSE)))
  expect_error(boolean_fill(list(FALSE), forward = FALSE))
  expect_error(boolean_fill(list(FALSE, list(list(FALSE, list(FALSE))))))
  expect_error(boolean_fill(list(FALSE, list(list(FALSE, list(FALSE)), forward = FALSE))))
})

test_that('it correctly fills in an atomic logical', {
  expect_identical(boolean_fill(c(FALSE, TRUE, FALSE)), c(FALSE, TRUE, TRUE))
  expect_identical(boolean_fill(c(FALSE, TRUE, FALSE), forward = FALSE),
                   c(TRUE, TRUE, FALSE))
})

test_that('it correctly fills in a simple list', {
  expect_identical(boolean_fill(list(FALSE, TRUE, FALSE)), list(FALSE, TRUE, TRUE))
  expect_identical(boolean_fill(list(FALSE, TRUE, FALSE), forward = FALSE),
                   list(TRUE, TRUE, FALSE))
})

test_that('it correctly fills in a nested list forwards', {
  expect_identical(boolean_fill(list(FALSE, list(FALSE, list(FALSE, TRUE, FALSE), FALSE), FALSE, list(FALSE, TRUE))),
                   list(FALSE, list(FALSE, list(FALSE, TRUE, TRUE), TRUE), TRUE, TRUE))
})

test_that('it correctly fills in a nested list backwards', {
  expect_identical(boolean_fill(list(FALSE, list(FALSE, FALSE), list(FALSE, list(FALSE, TRUE, FALSE), FALSE), list(FALSE, TRUE)), forward = FALSE),
                   list(TRUE, TRUE, list(TRUE, list(TRUE, TRUE, FALSE), FALSE), list(FALSE, TRUE)))

})

