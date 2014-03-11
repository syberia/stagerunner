context('nested names')

test_that('it correctly parses nested names for a small list', {
  expect_equal(nested_names(list(a = list(b = 1, c = list(d = 2, e = 3)), f = 4, 5)),
               c('a/b', 'a/c/d', 'a/c/e', 'f', '3'))
})


