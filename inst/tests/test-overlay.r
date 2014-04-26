context('stageRunner overlaying')

test_that('it can overlay a complicated example correctly', {
  #sr1 <- stageRunner$new(cx <- new.env(), list(c = function(x) x$x <- -1, a = function(x) { x$x <- 1; cat('setting x'); }, b = function(x) { x$x <- 2; cat('setting x to 2') }), remember = T)
  #sr2 <- stageRunner$new(cx, list(a = list(function(x) cat('..x is ', x$x), function(x) { cat('X WAS', cached_env$x)}), b = function(x) cat('..done!')))
  #sr1$overlay(sr2); sr1$run()
})
