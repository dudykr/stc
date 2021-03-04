# The analyzer

The typescript type checker.

# Directory structures

## `tests`

Single-file tests.

### `tests/pass`

Prints type and verifies that the file is validated without any error.
You should prefer `pass` testing over `visualize` testing.

### `tests/visualization`

Visualization without checking for errors.
