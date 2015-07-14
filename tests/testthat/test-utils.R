context("utils")

describe("as.ordinal", {
  test_that("it correctly turns some numbers into ordinals", {
    expect_equal(vapply(c(1, 10, 21, 22, 23, 24), as.ordinal, character(1)),
                 c("first", "tenth", "21st", "22nd", "23rd", "24th"))
  })
})

describe("enforce_type", {
  test_that("it fails if a key is missing", {
    expect_error((function(a) enforce_type(a, "logical"))(), "Please provide")
  })
})


