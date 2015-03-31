context('compare_stage_keys')

test_that('it correctly compares two empty lists', {
  expect_true(compare_stage_keys(list(), list()))
})

test_that('it correctly compares a simple linear list', {
  expect_true(compare_stage_keys(list(FALSE, TRUE, FALSE), list(FALSE, FALSE, TRUE)))
  expect_false(compare_stage_keys(list(FALSE, FALSE, TRUE), list(FALSE, TRUE, FALSE)))
})

test_that('it correctly compares a nested list to a linear list', {
  expect_true(compare_stage_keys(list(FALSE, list(FALSE, TRUE), FALSE), list(FALSE, FALSE, TRUE)))
  expect_true(compare_stage_keys(list(FALSE, list(TRUE, FALSE), FALSE), list(FALSE, TRUE, TRUE)))
  expect_true(compare_stage_keys(list(FALSE, list(list(TRUE, FALSE), FALSE), FALSE), list(FALSE, TRUE, TRUE)))
  expect_true(compare_stage_keys(list(FALSE, list(list(FALSE, TRUE), FALSE), FALSE), list(FALSE, TRUE, TRUE)))
  expect_false(compare_stage_keys(list(FALSE, FALSE, TRUE), list(FALSE, list(FALSE, TRUE), FALSE)))
  expect_false(compare_stage_keys(list(FALSE, list(FALSE, TRUE), TRUE), list(FALSE, list(TRUE, FALSE), FALSE)))
})

test_that('it correctly compares a nested list to a nested list', {
  expect_true(compare_stage_keys(list(FALSE, list(FALSE, FALSE), list(FALSE, list(TRUE, FALSE), TRUE)),
                                 list(FALSE, list(FALSE, FALSE), list(FALSE, list(FALSE, TRUE), TRUE))))
  expect_false(compare_stage_keys(list(FALSE, list(FALSE, FALSE), list(FALSE, list(FALSE, TRUE), TRUE)),
                                  list(FALSE, list(FALSE, FALSE), list(FALSE, list(TRUE, FALSE), TRUE))))
  expect_true(compare_stage_keys(list(list(TRUE)), list(list(FALSE, TRUE))))
  expect_true(compare_stage_keys(list(list(FALSE, TRUE)), list(list(TRUE))))
})

