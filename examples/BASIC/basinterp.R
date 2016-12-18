# This file provides the runtime support for running a basic program
# Assumes the program has been parsed using basparse.py

'%nin%' <- Negate('%in%')

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
    # Evaluate an expression
    eval = function(expr) {
      etype <- expr[[1]]
      if(etype == 'NUM') return(expr[[2]])
      else if(etype == 'GROUP') return(self$eval(expr[[2]]))
      else if(etype == 'UNARY') {
        if(expr[[2]] == '-') return(-self$eval(expr[[3]]))
      } else if(etype == 'BINOP') {
             if(expr[[2]] == '+') return(self$eval(expr[[3]]) + self$eval(expr[[4]]))
        else if(expr[[2]] == '-') return(self$eval(expr[[3]]) - self.eval(expr[[4]]))
        else if(expr[[2]] == '*') return(self$eval(expr[[3]]) * self.eval(expr[[4]]))
        else if(expr[[2]] == '/') return(float(self$eval(expr[[3]])) / self$eval(expr[[4]]))
        else if(expr[[2]] == '^') return(abs(self$eval(expr[[3]])) ^ self$eval(expr[[4]]))
      } else if(etype == 'VAR') {
        
      }
    },
    # Evaluate a relational expression
    releval = function(expr) {
      etype <- expr[[2]]
      lhs <- self$eval(expr[[3]])
      rhs <- self$eval(expr[[4]])
      if(etype == '<')
        if(lhs < rhs) return(TRUE)
        else          return(FALSE)
      else if(etype == '<=')
        if(lhs <= rhs) return(TRUE)
        else           return(FALSE)
      else if(etype == '>')
        if(lhs > rhs) return(TRUE)
        else          return(FALSE)
      else if(etype == '>=')
        if(lhs >= rhs) return(TRUE)
        else           return(FALSE)
      else if(etype == '=')
        if(lhs == rhs) return(TRUE)
        else           return(FALSE)
      else if(etype == '<>')
        if(lhs != rhs) return(TRUE)
        else           return(FALSE)
    },
    # Assignment
    assign = function(target, value) {
      var  <- target[[1]]
      dim1 <- target[[2]]
      dim2 <- target[[3]]
      if(is.na(dim1) && is.na(dim2)) self$vars[[var]] <- self$eval(value)
      else if(!is.na(dim1) && is.na(dim2)) {
        # List assignment
        dim1val <- self$eval(dim1)
        if(var %nin% self.lists) self$lists[[var]] <- c(0) * 10
        
        if(dim1val > length(self$lists[[var]])) {
          print(sprintf("DIMENSION TOO LARGE AT LINE %s", self$stat[[self$pc]]))
          stop()
        }
        self$lists[var][dim1val - 1] = self.eval(value)
      } else if(!is.na(dim1) && !is.na(dim2)) {
        dim1val <- self$eval(dim1)
        dim2val <- self$eval(dim2)
        if(var %nin% self$tables) {
          temp <- c(0) * 10
          v <- list()
          for(i in 1:10) v <- append(v, temp)
          self$tables[[var]] <- v
        }
        # Variable already exists
        if(dim1val > length(self$tables[[var]]) || dim2val > length(self$tables[[var]][[0]])) {
          print(sprintf("DIMENSION TOO LARGE AT LINE %s", self$stat[[self$pc]]))
          stop()
        }
        self$tables[[var]][[dim1val - 1]][[dim2val - 1]] <- self$eval(value)
      }
    },
    # Change the current line number
    goto = function(linenum) {
      
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
      self$pc <- 1                 # Current program counter

      # Processing prior to running
      
      self$collect_data()          # Collect all of the data statements
      self$check_end()
      self$check_loops()
      
      if(self$error) stop()
      
      while(TRUE) {
        line <- self$stat[[self$pc]]
        instr <- self$prog[[as.character(line)]]
        
        op <- instr[[1]]
        
        # END and STOP statements
        if(op == 'END' || op == 'STOP') break # We're done
        
        # GOTO statement
        else if(op == 'GOTO') {
          newline <- instr[[2]]
          self$goto(newline)
          next
        }
        
        # PRINT statement
        else if(op == 'PRINT') {
          plist <- instr[[2]]
          out <- ""
          for(label_val in plist) {
            label <- label_val[[1]]
            val   <- label_val[[2]]
            if(nchar(out) > 0) out <- paste(out, sprintf('%-15s'))
            out <- paste(out, label, sep='')
            if(!is.null(val)) {
              if(!is.null(label)) out <- paste(out, " ", sep='')
              eval = self.eval(val)
              out <- paste(out, toString(eval))
            }
          }
          cat(out)
          end <- instr[[3]]
          if(!is.null(end)) {
            if(!(end == ',' || end == ';')) cat("\n")
            if(end == ',') cat(" ")
            if(end == ';') cat(" ")
          }
        }
        
        # ...
  
        else if(op == 'FOR') {
          loopvar <- instr[[2]]
          initval <- instr[[3]]
          finval  <- instr[[4]]
          stepval <- instr[[5]]
          
          # Check to see if this is a new loop
          if(length(self$loops) == 0 || tail(self$loops, 1)[[1]][[1]] != self$pc) {
            # Looks like a new loop. Make the initial assignment
            newvalue <- initval
            self$assign(list(loopvar, NA, NA), initval)
            if(is.null(stepval)) stepval <- c('NUM', 1)
            stepval < self$eval(stepval)    # Evaluate step here
            self$loops[[length(self$loops)+1]] <- c(self$pc, stepval)
          } else {
            # It's a repeat of the previous loop
            # Update the value of the loop variable according to the
            # step
            stepval <- list('NUM', tail(self$loops, 1)[[1]][[2]])
            newvalue <- list('BINOP', '+', list('VAR', list(loopvar, NA, NA)), stepval)
          }
          
          if(tail(self$loops, 1)[[1]][[2]] < 0) relop <- '>='
          else                                  relop <- '<='
          if(!self$releval(list('RELOP', relop, newvalue, finval))) {
            # Loop is done. Jump to the NEXT
            self$pc <- self$loopend[[self$pc]]
            self$loops <- head(self$loops, -1)
          } else self$assign(list(loopvar, NA, NA), newvalue)
        }
  
        else if(op == 'NEXT') {
          if(length(self$loops) == 0) {
            print(sprintf("NEXT WITHOUT FOR AT LINE %s", line))
            return()
          }
          
          nextvar <- instr[[2]]
          self$pc <- tail(self$loops, 1)[[1]][[1]]
          loopinst <- self$prog[[self$stat[[self$pc]]]]
          forvar <- loopinst[[2]]
          if(nextvar != forvar) {
            print(sprintf("NEXT DOESN'T MATCH FOR AT LINE %s", line))
            return()
          }
          next
        }
        # ...
  
        else if(op == 'FUNC') {
          fname <- instr[[2]]
          pname <- instr[[3]]
          expr  <- instr[[4]]
          
          eval_func = function(pvalue, name=pname, self=self, expr=expr) {
#            self.assign((pname, None, None), pvalue)
#            return self.eval(expr)
          }
          self$functions[[fname]] <- eval_func
        }
    
        else if(op == 'DIM') {
          # ...
        }
        
        self$pc <- self$pc + 1
      }
    }
  )
)