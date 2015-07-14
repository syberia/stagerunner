context('stageRunner mode parameter')

test_that('it does not allow invalid values for the mode parameter', {
  expect_error(stageRunner(new.env(), list(), remember = TRUE, mode = 'bad'),
               "should be")
  expect_error(stageRunner(new.env(), list(), remember = TRUE, mode = NULL),
               "Please pass")
})

test_that('it allows "head" and "next" as values for the mode parameter', {
  stageRunner(new.env(), list(), remember = TRUE, mode = 'head')
  stageRunner(new.env(), list(), remember = TRUE, mode = 'next')
})

test_that('it correctly resumes when using "next" mode', {
  tmp <- new.env()
  x <- 0; y <- 0; z <- 0
  sr <- stageRunner(tmp, list(function(e) x <<- 1, function(e) y <<- 1,
                              function(e) z <<- 1), remember = TRUE, mode = 'next')
  sr$run(1)
  x <- NULL
  sr$run()
  expect_identical(x, NULL)
  expect_identical(y, 1)
  expect_identical(z, 1)
})

test_that('it correctly resumes when using "next" mode and stops given the "to" parameter', {
  tmp <- new.env()
  x <- 0; y <- 0; z <- 0; w <- 0
  sr <- stageRunner(tmp, list(function(e) x <<- 1, function(e) y <<- 1,
                              function(e) z <<- 1, function(e) w <<- 1),
    remember = TRUE, mode = 'next')
  sr$run(1)
  x <- NULL
  sr$run(to = 3)
  expect_identical(x, NULL)
  expect_identical(y, 1)
  expect_identical(z, 1)
  expect_identical(w, 0)
})


