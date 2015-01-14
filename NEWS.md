# Version 0.4.0

* Integration with the [objectdiff package](http://github.com/robertzk/objectdiff).
  This means passing a `tracked_environment` object as the `context` to
  a `stageRunner` will result in much smarter memory management of environment
  changes as you run its stages (objectdiff uses clever patching functions to
  record only *changes* to an environment as it is being modified; without this
  logic, a stageRunner has to copy an environment in full on each executed
  stage, which is especially expensive if you are manipulating large data sets).

# Version 0.3.2

* Stagerunners now contain an `$around` method that allows for
  wrapping terminal stages with some behavior, akin to the
  `yield` keyword in Ruby blocks. This can be
  useful for interim debugging, visualization, etc. For more
  information see `?stageRunner__around`.

