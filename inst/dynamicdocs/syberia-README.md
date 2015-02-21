## Stagerunner

**[Stagerunner](http://github.com/robertzk/stagerunner)** is the core object responsible
for running models. The native workflow for a typical R programmer when processing data
  or playing with parameters is to re-execute files or pieces of files. While functional,
  this approach has a few drawbacks. The process of re-executing parts manually encourages
  code pollution through debugging / print statements and impacts long-term maintainability
  without a good habit of reverting these changes.

  It is difficult to know what parts to execute to achieve a specific outcome without
  reading the code in detail: if I know a model file imputes several variables, and I am
  debugging an issue I believe is related to this imputation, I have go to find which
  part is responsible first.

  It is difficult to organize the script in any canonical fashion other than through
  comment sections. Even if the correct organization is hierarchical, a file-based
  approach always encourages a flat linear structure.

  Working with `stageRunner` objects solves these issues. A `stageRunner` is merely a
  nested list of functions that each take one argument: [an environment](http://adv-r.had.co.nz/Environments.html)
  (you should be familiar with the R environment data structure). This environment
  is the "playground" for the functions, and as you pass through each one, you should
  be modifying this environment according to what you'd like to preserve across each
  step. For example, importing data should create a `data` variable in the environment,
  and modifying the data should modify this `data` variable in this environment.

  Behind the scenes, a `stageRunner` keeps track of every modification to the
  environment it is attached to (which we from now on refer to as its "context").
  You can "replay" these changes when debugging; if you are manipulating some data and reach
  the tenth step of data preparation and your data looks wrong, you can go back and
  look at what it was like in steps 1-9 without having to re-execute code from
  the beginning. For a more detailed example of how to do this,
  take a look at the [stageRunner interactive tutorial](http://en.wikipedia.org/wiki/Vaporware)
  (**TODO**: Make this.)
