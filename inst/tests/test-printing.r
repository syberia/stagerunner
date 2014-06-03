context('printing stageRunner')

local({
  unnamed <- function(n) paste0(' < Unnamed \\(stage ', n, ') > ')
  wrap <- function(..., prefix = '') {
    args <- list(...)
    n <- args[[length(args)]]
    txt <- paste0(args[-length(args)], collapse = '')
    paste0("A", prefix, " stageRunner with ", n, " stages:[\n\r]*", txt,
           "[\n\r]*Context <environment: [^>]+>")
  }

  test_that('it correctly prints a trivial stageRunner', {
    sr <- stageRunner$new(new.env(), list())
    expect_output(print(sr), wrap('', 0))
  })

  test_that('it correctly prints a one-stage stageRunner with an unnamed stage', {
    sr <- stageRunner$new(new.env(), list(force))
    expect_output(print(sr), wrap(' -', unnamed(1), 1))
  })

  test_that('it correctly prints an executed one-stage stageRunner with an unnamed stage', {
    sr <- stageRunner$new(new.env(), list(force)); sr$run()
    expect_output(print(sr), wrap(' -', unnamed(1), 1)) # should be same as non-executed version
  })

  test_that('it correctly prints a one-stage stageRunner with a named stage', {
    sr <- stageRunner$new(new.env(), list(one = force))
    expect_output(print(sr), wrap(" - one ", 1))
  })

  test_that('it correctly prints a nested stagerunner with one superstage and two unnamed substages', {
    sr <- stageRunner$new(new.env(), list(one = list(force, force))); sr$run()
    expect_output(print(sr),
      wrap(" - one [\n\r]*   -", unnamed(1), " [\n\r]*  -", unnamed(2), 2))
  })

  test_that('it correctly prints a nested stagerunner with one superstage and two named substages', {
    sr <- stageRunner$new(new.env(), list(one = list(two = force, three = force))); sr$run()
    expect_output(print(sr),
      wrap(" - one [\n\r]*   - two [\n\r]*   - three ", 2))
  })

  test_that('it correctly prints a nested stagerunner with one superstage, one named substage, and one unnamed substage', {
    sr <- stageRunner$new(new.env(), list(one = list(two = force, force))); sr$run()
    expect_output(print(sr),
      wrap(" - one [\n\r]*   - two [\n\r]*   -", unnamed(2), 2))
  })

  test_that('it correctly prints a nested stagerunner two flat stages wrapping a nested stage, all named', {
    sr <- stageRunner$new(new.env(), list(a = force, b = list(c = force, d = force), e = force))
    sr$run()
    expect_output(print(sr),
      wrap(" - a [\n\r]* - b [\n\r]*   - c [\n\r]*   - d [\n\r]* - e ", 4))
  })

  context('printing stageRunner with caching') 

  wrapc <- function(...) wrap(..., prefix = ' caching')

  test_that('it correctly prints a trivial caching stageRunner', {
    sr <- stageRunner$new(new.env(), list(), remember = TRUE)
    expect_output(print(sr), wrapc('', 0))
  })

  test_that('it correctly prints a one-stage caching stageRunner with an unnamed stage', {
    sr <- stageRunner$new(new.env(), list(force), remember = TRUE)
    expect_output(print(sr), wrapc(' \\*', unnamed(1), 1))
  })

  test_that('it correctly prints a one-stage caching stageRunner with an unnamed stage after execution', {
    sr <- stageRunner$new(new.env(), list(force), remember = TRUE); sr$run()
    expect_output(print(sr), wrapc(' \\+', unnamed(1), 1))
  })

  test_that('it correctly prints a two-stage caching stageRunner with unnamed stages after execution', {
    sr <- stageRunner$new(new.env(), list(force, force), remember = TRUE); sr$run()
    expect_output(print(sr),
      wrapc(' \\+', unnamed(1), '[\n\r]* \\+', unnamed(2), 2))
  })

  test_that('it correctly prints a two-stage caching stageRunner with nested stages', {
    sr <- stageRunner$new(new.env(), list(a = force, b = list(c = force, d = force)), remember = TRUE)
    expect_output(print(sr), wrapc(' \\* a [\n\r]* - b [\n\r]*   - c [\n\r]*   - d ', 3))
  })

  test_that('it correctly prints a two-stage caching stageRunner with nested stages after partial execution', {
    sr <- stageRunner$new(new.env(), list(a = force, b = list(c = force, d = force)), remember = TRUE)
    sr$run(1)
    expect_output(print(sr), wrapc(' \\+ a [\n\r]* \\* b [\n\r]*   \\* c [\n\r]*   - d ', 3))
  })

  test_that('it correctly prints a two-stage caching stageRunner with nested stages after execution', {
    sr <- stageRunner$new(new.env(), list(a = force, b = list(c = force, d = force)), remember = TRUE)
    sr$run()
    expect_output(print(sr), wrapc(' \\+ a [\n\r]* \\+ b [\n\r]*   \\+ c [\n\r]*   \\+ d ', 3))
  })

  test_that('it correctly prints a two-stage caching stageRunner with nested stages after executing all but the last stage', {
    sr <- stageRunner$new(new.env(), list(a = force, b = list(c = force, d = force)), remember = TRUE)
    sr$run(1, to = '2/1')
    expect_output(print(sr), wrapc(' \\+ a [\n\r]* \\* b [\n\r]*   \\+ c [\n\r]*   \\* d ', 3))
  })

  test_that('it correctly prints a two-stage caching stageRunner with nested stages after executing all but the penultimate stage', {
    sr <- stageRunner$new(new.env(), list(a = force, b = list(c = force, d = force, e = force)), remember = TRUE)
    sr$run(1, to = '2/1')
    expect_output(print(sr), wrapc(' \\+ a [\n\r]* \\* b [\n\r]*   \\+ c [\n\r]*   \\* d [\n\r]*   \\- e ', 4))
  })

})

