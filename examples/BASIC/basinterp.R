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
    dc   = NA,
    
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
        if(self$prog[[as.character(lineno)]][1] == 'DATA') {
          self$data <- self$data + self$prog[[as.character(lineo)]][[2]]
        }
      }
      self$dc <- 0
    },
    # Check for end statements
    check_end = function() {
      has_end <- NULL
      for(lineno in self$stat)
        if(self$prog[[as.character(lineno)]][1] == 'END' && is.null(has_end))
          has_end <- lineno
          
      if(is.null(has_end)) {
        print("NO END INSTRUCTION")
        self$error <- TRUE
        return
      }

      if(has_end != lineno) {
        print("END IS NOT LAST")
        self$error <- TRUE
      }
    },
    # Check loops
    check_loops = function() {
      for(pc in 1:length(self$stat)) {
        lineno <- self$stat[[pc]]
        if(self$prog[[as.character(lineno)]][1] == 'FOR') {
          forinst <- self$prog[[as.character(lineno)]]
          loopvar <- forinst[[2]]
          broke <- FALSE
          for(i in (pc + 1):length(self$stat))
            if(self$prog[[as.character(self$stat[[i]])]][[1]] == 'NEXT') {
              nextvar <- self$prog[[as.character(self$stat[[i]])]][[2]]
              if(nextvar != loopvar) next
              self$loopend[[as.character(pc)]] <- i
              broke <- TRUE
              break
            }
          
          if(!broke) {
            print(sprintf("FOR WITHOUT NEXT AT LINE %s", self$stat[[as.character(pc)]]))
            self$error <- TRUE
          }
        }
      }
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
      
      self$stat <- names(self$prog) # Ordered list of all line numbers
      self$stat <- sort(sapply(self$stat, strtoi))
      self$pc <- 0                 # Current program counter

      # Processing prior to running
      
      self$collect_data()          # Collect all of the data statements
      self$check_end()
      self$check_loops()
      
    }
  )
)