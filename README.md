# codeworld-tasks

This library provides abstractions and functions for evaluating images created in the vector graphics domain CodeWorld.
The interface is mostly used as part of a Haskell programming task type on our e-learning platform [Autotool](https://git.uni-due.de/fmi/autotool-dev).

Documentation is available on [GitHub Pages](https://fmidue.github.io/codeworld-tasks/).

## Running Tasks Offline

The package provides an executable `test-task` to replicate the grading process of Autotool in a terminal.
This currently only works on Unix systems (OSX untested, but likely works), **Microsoft Windows is not supported**.

First, follow the usual Student workflow:

1. Copy a task from `examples/tasks` into the [CodeWorld editor](https://code.world/haskell).
1. Work on the task until you think you've solved it.
1. Paste your solution back into the template.

Now to simulate the grading process:

1. Install the z3 theorem prover (`sudo apt-get install libz3-dev` or similar)
1. Install [Haskell Stack](https://docs.haskellstack.org/en/stable/#__tabbed_2_1)
1. Optionally set an alias for `stack run -w run.yaml`
1. Execute `stack run -w run.yaml examples/configs/<Task> <examples/tasks/<Task>`

The submission will either be rejected or accepted and feedback be printed directly into the console.
Running the stack command may take a while the first time, since a lot of dependencies will have to be installed.

Sample solutions for all tasks are provided in `examples/solutions` for reference.
