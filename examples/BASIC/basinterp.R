# This file provides the runtime support for running a basic program
# Assumes the program has been parsed using basparse.py

BasicInterpreter <- R6Class("BasicInterpreter",
  public = list(
    # Initialize the interpreter. prog is a dictionary
    # containing (line,statement) mappings
    initialize = function(prog=NA) {
    }
  )
)