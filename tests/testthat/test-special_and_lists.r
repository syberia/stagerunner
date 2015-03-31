context("special_and_lists function")

test_that("it correctly ANDs two logicals of length 1", {
  expect_identical(special_and_lists(TRUE, TRUE), TRUE)
  expect_identical(special_and_lists(TRUE, FALSE), FALSE)
  expect_identical(special_and_lists(FALSE, TRUE), FALSE)
  expect_identical(special_and_lists(FALSE, FALSE), FALSE)
})

test_that("it correctly ANDs a list with a logical of length 1", {
  example_list <- list(a = TRUE, b = list(b = FALSE, c = TRUE))
  expect_identical(special_and_lists(TRUE, example_list), example_list)
  expect_identical(special_and_lists(FALSE, example_list), FALSE)
})

test_that("it correctly ANDs a list with another list over an atomic difference", {
  example_list <- list(a = TRUE, b = list(b = FALSE, c = TRUE))
  example_list2 <- list(a = FALSE, b = list(b = FALSE, c = TRUE))
  expect_identical(special_and_lists(example_list, example_list2), example_list2)
})

test_that("it correctly ANDs a list with another list over a nested atomic difference", {
  example_list <- list(a = TRUE, b = list(b = FALSE, c = FALSE))
  example_list2 <- list(a = FALSE, b = list(b = FALSE, c = TRUE))
  correct_list <- list(a = FALSE, b = list(b = FALSE, c = FALSE))
  expect_identical(special_and_lists(example_list, example_list2), correct_list)
})

test_that("it correctly ANDs a list with another list over a list difference", {
  example_listT <- list(a = TRUE, b = list(b = FALSE, c = FALSE))
  example_listF <- list(a = FALSE, b = list(b = FALSE, c = FALSE))
  example_list2 <- list(a = list(b = TRUE, c = TRUE), b = list(b = FALSE, c = TRUE))
  correct_list <- list(a = list(b = TRUE, c = TRUE), b = list(b = FALSE, c = FALSE))
  expect_identical(special_and_lists(example_listT, example_list2), correct_list)
  expect_identical(special_and_lists(example_listF, example_list2), example_listF)
})

test_that("it correctly ANDs a list with another list over a nested list difference", {
  example_list <- list(a = FALSE, b = list(b = FALSE, c = TRUE))
  example_list2 <- list(a = FALSE, b = list(b = FALSE, c = list(d = TRUE, e = FALSE)))
  expect_identical(special_and_lists(example_list, example_list2), example_list2)
})

test_that("it has an error if two lists dont match in length", {
  example_list <- list(TRUE, TRUE, FALSE)
  example_list2 <- list(FALSE, FALSE)
  expect_error(special_and_lists(example_list, example_list2),
    info = "special_and_lists should throw an error if two lists dont match in length")
})

test_that("lists get ANDed by order, not by name", {
  example_list <- list(a = TRUE, b = FALSE)
  example_list2 <- list(b = FALSE, a = TRUE)
  expect_warning(out <- special_and_lists(example_list, example_list2),
    "names of the two lists do not match",
    info = 'special_and_lists must throw a warning if list names do not match')
  expect_identical(out, list(a = FALSE, b = FALSE))
})

test_that("it does a really complicated example correctly", {
  example_list <- list(a = TRUE, b = FALSE, c = list(d = list(a = FALSE, b = TRUE), e = list(f = TRUE, g = FALSE)), d = FALSE)
  example_list2 <- list(a = list(a = TRUE, b = FALSE), b = TRUE, c = list(d = list(a = TRUE, b = TRUE), e = TRUE), d = list(a = TRUE))
  correct_list <- list(a = list(a = TRUE, b = FALSE), b = FALSE, c = list(d = list(a = FALSE, b = TRUE), e = list(f = TRUE, g = FALSE)), d = FALSE)
  expect_identical(special_and_lists(example_list, example_list2), correct_list)
})

test_that("it preserves names according to the first list", {
  example_list <- list(a = TRUE, b = FALSE)
  example_list2 <- list(b = FALSE, a = TRUE)
  expect_identical(
    names(suppressWarnings(special_and_lists(example_list, example_list2))), c("a", "b"),
    info = "special_and_lists must preserve names according to the first list")
})

