context("special_or_lists function")

test_that("it correctly ORs two logicals of length 1", {
  expect_identical(special_or_lists(TRUE, TRUE), TRUE)
  expect_identical(special_or_lists(TRUE, FALSE), TRUE)
  expect_identical(special_or_lists(FALSE, TRUE), TRUE)
  expect_identical(special_or_lists(FALSE, FALSE), FALSE)
})

test_that("it correctly ORs a list with a logical of length 1", {
  example_list <- list(a = TRUE, b = list(b = FALSE, c = TRUE))
  expect_identical(special_or_lists(TRUE, example_list), TRUE)
  expect_identical(special_or_lists(FALSE, example_list), example_list)
})

test_that("it correctly ORs a list with another list over an atomic difference", {
  example_list <- list(a = TRUE, b = list(b = FALSE, c = TRUE))
  example_list2 <- list(a = FALSE, b = list(b = FALSE, c = TRUE))
  expect_identical(special_or_lists(example_list, example_list2), example_list)
})

test_that("it correctly ORs a list with another list over a nested atomic difference", {
  example_list <- list(a = TRUE, b = list(b = FALSE, c = FALSE))
  example_list2 <- list(a = FALSE, b = list(b = FALSE, c = TRUE))
  correct_list <- list(a = TRUE, b = list(b = FALSE, c = TRUE))
  expect_identical(special_or_lists(example_list, example_list2), correct_list)
})

test_that("it correctly ORs a list with another list over a list difference", {
  example_listT <- list(a = TRUE, b = list(b = FALSE, c = FALSE))
  example_listF <- list(a = FALSE, b = list(b = FALSE, c = FALSE))
  example_list2 <- list(a = list(b = TRUE, c = TRUE), b = list(b = FALSE, c = TRUE))
  correct_list <- list(a = TRUE, b = list(b = FALSE, c = TRUE))
  expect_identical(special_or_lists(example_listT, example_list2), correct_list)
  expect_identical(special_or_lists(example_listF, example_list2), example_list2)
})

test_that("it correctly ORs a list with another list over a nested list difference", {
  example_list <- list(a = FALSE, b = list(b = FALSE, c = FALSE))
  example_list2 <- list(a = FALSE, b = list(b = FALSE, c = list(d = TRUE, e = FALSE)))
  expect_identical(special_or_lists(example_list, example_list2), example_list2)
})

test_that("it has an error if two lists dont match in length", {
  example_list <- list(TRUE, TRUE, FALSE)
  example_list2 <- list(FALSE, FALSE)
  expect_error(special_or_lists(example_list, example_list2),
    info = "special_or_lists should throw an error if two lists dont match in length")
})

test_that("lists get ORed by order, not by name", {
  example_list <- list(a = TRUE, b = FALSE)
  example_list2 <- list(b = FALSE, a = TRUE)
  expect_warning(out <- special_or_lists(example_list, example_list2),
    "names of the two lists do not match",
    info = 'special_or_lists must throw a warning if list names do not match')
  expect_identical(out, list(a = TRUE, b = TRUE))
})

test_that("it does a really complicated example correctly", {
  example_list <- list(a = TRUE, b = FALSE, c = list(d = list(a = FALSE, b = TRUE), e = list(f = TRUE, g = FALSE)), d = FALSE)
  example_list2 <- list(a = list(a = TRUE, b = FALSE), b = TRUE, c = list(d = list(a = TRUE, b = FALSE), e = TRUE), d = list(a = TRUE))
  correct_list <- list(a = TRUE, b = TRUE, c = list(d = list(a = TRUE, b = TRUE), e = TRUE), d = list(a = TRUE))
  expect_identical(special_or_lists(example_list, example_list2), correct_list)
})

test_that("it preserves names according to the first list", {
  example_list <- list(a = TRUE, b = FALSE)
  example_list2 <- list(b = FALSE, a = TRUE)
  expect_identical(
    names(suppressWarnings(special_or_lists(example_list, example_list2))), c("a", "b"),
    info = "special_or_lists must preserve names according to the first list")
})

test_that("it errors on invalid list or atomic logical formats", {
  expect_error(special_or_lists(force, force), "only accepts")
})

test_that("it gives the correct result when the latter is FALSE", {
  expect_identical(special_or_lists(el1 <- list(list(FALSE, TRUE)), FALSE), el1)
})

