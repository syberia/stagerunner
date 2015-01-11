library(testthatsomemore)
library(objectdiff)

context('tracked_environment')

# The objectdiff package provides the ability to use a single environment
# to store a sequence of changes, with a Git-like ability to "roll back"
# to previous versions of the environment. It does this without extraneous
# memory overhead using clever heuristics.

# If a stageRunner is provided a tracked_environment instead of a regular
# environment, it will assume this behavior into its functioning so that
# if its remember parameter is set to TRUE, it will use the tracked_environment's
# rollback abilities to provide the correct behavior when executing an
# earlier stage.

describe('tracked_environments', {
  test_that('it can initialize a stageRunner using a tracked_environment', {
    assert(stageRunner$new(tracked_environment(), list()))
  })
})

