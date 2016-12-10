# This file provides the runtime support for running a basic program
# Assumes the program has been parsed using basparse.py

BasicInterpreter <- R6Class("BasicInterpreter",
  public = list(
    prog      = NA,
    functions = NA,
    
    vars    = NA,
    lists   = NA,
    tables  = NA,
    loops   = NA,
    loopend = NA,
    gosub   = NA,
    error   = NA,
    
    stat = NA,
    pc   = NA,
    
    data = NA,

    # Initialize the interpreter. prog is a dictionary
    # containing (line,statement) mappings
    initialize = function(prog=NA) {
      self$prog <- prog
      
      self$functions <- new.env(hash=TRUE) # Built-in function table
      self$functions[['SIN']] <- function(z) sin(eval(z))
      self$functions[['COS']] <- function(z) cos(eval(z))
      self$functions[['TAN']] <- function(z) tan(eval(z))
      self$functions[['ATN']] <- function(z) atan(eval(z))
      self$functions[['EXP']] <- function(z) exp(eval(z))
      self$functions[['ABS']] <- function(z) abs(eval(z))
      self$functions[['LOG']] <- function(z) log(eval(z))
      self$functions[['SQR']] <- function(z) sqrt(eval(z))
      self$functions[['INT']] <- function(z) strtoi(eval(z))
      self$functions[['RND']] <- function(z) runif(1)
    },
    # Collect all data statements
    collect_data = function() {
      self$data <- list()
      for(lineno in self$stat) {
        if(self$prog[[lineno]][1] == 'DATA') {
          self$data <- self$data + self$prog[[lineo]][[2]]
        }
      }
      self$dc <- 0
    },
    # Run it
    run = function() {
      self$vars    <- new.env(hash=TRUE) # All variables
      self$lists   <- new.env(hash=TRUE) # List variables
      self$tables  <- new.env(hash=TRUE) # Tables
      self$loops   <- list()             # Currently active loops
      self$loopend <- new.env(hash=TRUE) # Mapping saying where loops end
      self$gosub   <- NA                 # Gosub return point (if any)
      self$error   <- FALSE              # Indicates program error
      
      self$stat <- list(self$prog) # Ordered list of all line numbers
#      self$stat.sort()
      self$pc <- 0                 # Current program counter
      
      # Processing prior to running
      
#      self$collect_data()          # Collect all of the data statements
#      self$check_end()
#      self$check_loops()
      
    }
  )
)