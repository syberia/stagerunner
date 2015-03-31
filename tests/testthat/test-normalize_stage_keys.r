context('normalize_stage_keys')

test_that('it grabs all stages when passed no keys parameter', {
  expect_identical(normalize_stage_keys(NULL, vector('list', 10)), rep(list(TRUE), 10))
  expect_identical(normalize_stage_keys(list(), vector('list', 10)), rep(list(TRUE), 10))
})

test_that('it leaves all logical lists untouched', {
  tmp <- list(TRUE, FALSE)
  expect_identical(normalize_stage_keys(tmp, vector('list', 2)), tmp)
  tmp <- list(TRUE, list(TRUE, FALSE))
  expect_identical(normalize_stage_keys(tmp, vector('list', 2)), tmp)
})

test_that('it allows negative indexing', {
  expect_identical(normalize_stage_keys(-(1:2), vector('list', 3)), list(FALSE, FALSE, TRUE))
})

test_that('it allows numeric indexing', {
  expect_identical(normalize_stage_keys(1:2, vector('list', 3)), list(TRUE, TRUE, FALSE))
  expect_identical(normalize_stage_keys(3, vector('list', 3)), list(FALSE, FALSE, TRUE))
})

test_that('it errors on empty keys', {
  expect_error(normalize_stage_keys(list(list()), list()), 'cannot be of length 0')
})

test_that('it can nest using logicals', {
  # expect_identical(normalize_stage_keys(list(2, list(FALSE, TRUE)), list(1, list(2, 3))), list(FALSE, list(FALSE, TRUE)))
  # What was I doing here..?
})

test_that('it can nest using character keys', {
  expect_identical(normalize_stage_keys(list('a/b', 'c/d'), list(a = list(b = 1,2),c = list(1,d = 2))),
                   list(list(TRUE, FALSE), list(FALSE, TRUE)))
  expect_identical(normalize_stage_keys(list('a/b/c'), list(a = list(b = list(1, c = 2)), 3)),
                   list(list(list(FALSE, TRUE)), FALSE))
})

test_that('it errors on nonexistent keys', {
  expect_error(normalize_stage_keys(list('a/b/c/d'), list(a = list(b = list(1, c = list(e = 1))), 3)),
               "No stage with key 'a/b/c/d' found")
})

test_that('it prefers numeric to matching', {
  expect_identical(normalize_stage_keys('1', list(one = 1, 'this 1' = 2)), list(TRUE, FALSE))
})

test_that('it prefers numeric to matching on nested references', {
  expect_identical(
    normalize_stage_keys('two/1', list(uno = 1, two = list(one = 1, 'this 1' = 2))),
    list(FALSE, list(TRUE, FALSE)))
})

test_that('it correctly handles a to parameter in a nested setting', {
  keys <- normalize_stage_keys('one/two', to = 'one',
    list(one = list(one = 1, two = 2, three = 3)))

  expect_identical(keys, list(list(FALSE, TRUE, TRUE)))
})


