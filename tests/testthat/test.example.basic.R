#! /usr/bin/env Rscript

library(rly)

KEYWORDS = c('LET', 'READ', 'DATA', 'PRINT', 'GOTO', 'IF', 'THEN', 'FOR', 'NEXT', 'TO', 'STEP',
             'END', 'STOP', 'DEF', 'GOSUB', 'DIM', 'REM', 'RETURN', 'RUN', 'LIST', 'NEW')
TOKENS = c(KEYWORDS, c('EQUALS', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'POWER',
                       'LPAREN', 'RPAREN', 'LT', 'LE', 'GT', 'GE', 'NE',
                       'COMMA', 'SEMI', 'INTEGER', 'FLOAT', 'STRING',
                       'ID', 'NEWLINE'))

Lexer <- R6::R6Class("Lexer",
  public = list(
    tokens = TOKENS,
    t_ignore = " \t",
    t_REM = function(re='REM .*', t) {
      return(t)
    },
    t_ID = function(re='[A-Z][A-Z0-9]*', t) {
      if(t$value %in% KEYWORDS) t$type <- t$value
      return(t)
    },
    t_EQUALS  = '=',
    t_PLUS    = '\\+',
    t_MINUS   = '-',
    t_TIMES   = '\\*',
    t_POWER   = '\\^',
    t_DIVIDE  = '/',
    t_LPAREN  = '\\(',
    t_RPAREN  = '\\)',
    t_LT      = '<',
    t_LE      = '<=',
    t_GT      = '>',
    t_GE      = '>=',
    t_NE      = '<>',
    t_COMMA   = '\\,',
    t_SEMI    = ';',
    t_INTEGER = '\\d+',
    t_FLOAT   = '((\\d*\\.\\d+)(E[\\+-]?\\d+)?|([1-9]\\d*E[\\+-]?\\d+))',
    t_STRING  = '\\".*?\\"',
    t_NEWLINE = function(re='\\n', t) {
      t$lexer$lineno <- t$lexer$lineno + 1
      return(t)
    },
    t_error = function(t) {
      cat(sprintf("Illegal character '%s'", t$value[[1]]))
      t$lexer$skip(1)
      return(NULL)
    }
  )
)

Parser <- R6::R6Class("Parser",
  public = list(
    tokens = TOKENS,
    # Parsing rules
    precedence = list(c('left', 'PLUS', 'MINUS'),
                      c('left', 'TIMES', 'DIVIDE'),
                      c('left', 'POWER'),
                      c('right', 'UMINUS')),
    # A BASIC program is a series of statements.  We represent the program as a
    # dictionary of tuples indexed by line number.
    p_program = function(doc='program : program statement
                                      | statement', p) {
      if(p$length() == 2 && !is.null(p$get(2))) {
        p1 <- new.env(hash=TRUE)
        line <- p$get(2)[[1]]
        stat <- p$get(2)[[2]]
        p1[[as.character(line)]] <- stat
        p$set(1, p1)
      } else if(p$length() == 3) {
        p$set(1, p$get(2))
        if(is.null(p$get(1)))  p$set(1, new.env(hash=TRUE))
        if(!is.null(p$get(3))) {
          line <- p$get(3)[[1]]
          stat <- p$get(3)[[2]]
          p1 <- p$get(1)
          p1[[as.character(line)]] <- stat
          p$set(1, p1)
        }
      }
    },
    # This catch-all rule is used for any catastrophic errors.  In this case,
    # we simply return nothing
    p_program_error = function(doc='program : error', p) {
      p$set(1, NULL)
      p$parser$error <- 1
    },
    # Format of all BASIC statements.
    p_statement = function(doc='statement : INTEGER command NEWLINE', p) {
      if(typeof(p$get(3)) == "character") {
        cat(sprintf("%s %s %s", p$get(3), "AT LINE", p$get(2)))
        p$set(1, NULL)
        p$parser$error <- 1
      } else {
        lineno <- strtoi(p$get(2))
        p$set(1, list(lineno, p$get(3)))
      }
    },
    # Interactive statements.
    p_statement_interactive = function(doc='statement : RUN NEWLINE
                                                      | LIST NEWLINE
                                                      | NEW NEWLINE', p) {
      p$set(1, list(0, list(p$get(2), 0)))
    },
    # Blank line number
    p_statement_blank = function(doc='statement : INTEGER NEWLINE', p) {
      p$set(1, list(0, list('BLANK', strtoi(p$get(2)))))
    },
    # Error handling for malformed statements
    p_statement_bad = function(doc='statement : INTEGER error NEWLINE', p) {
      cat(sprintf("MALFORMED STATEMENT AT LINE %s", p$get(2)))
      p$set(1, NULL)
      p$parser$error <- 1
    },
    # Blank line
    p_statement_newline = function(doc='statement : NEWLINE', p) {
      p$set(1, NULL)
    },
    # LET statement
    p_command_let = function(doc='command : LET variable EQUALS expr', p) {
      p$set(1, list('LET', p$get(3), p$get(5)))
    },
    p_command_let_bad = function(doc='command : LET variable EQUALS error', p) {
      p$set(1, "BAD EXPRESSION IN LET")
    },
    # READ statement
    p_command_read = function(doc='command : READ varlist', p) {
      p$set(1, list('READ', p$get(3)))
    },
    p_command_read_bad = function(doc='command : READ error', p) {
      p$set(1, "MALFORMED VARIABLE LIST IN READ")
    },
    # DATA statement
    p_command_data = function(doc='command : DATA numlist', p) {
      p$set(1, list('DATA', p$get(3)))
    },
    p_command_data_bad = function(doc='command : DATA error', p) {
      p$set(1, "MALFORMED NUMBER LIST IN DATA")
    },
    # PRINT statement
    p_command_print = function(doc='command : PRINT plist optend', p) {
      p$set(1, list('PRINT', p$get(3), p$get(4)))
    },
    p_command_print_bad = function(doc='command : PRINT error', p) {
      p$set(1, "MALFORMED PRINT STATEMENT")
    },
    # Optional ending on PRINT. Either a comma (,) or semicolon (;)
    p_optend = function(doc='optend : COMMA 
                                    | SEMI
                                    |', p) {
      if(p$length() == 2) p$set(1, p$get(2))
      else                p$set(1, NULL)
    },
    # PRINT statement with no arguments
    p_command_print_empty = function(doc='command : PRINT', p) {
      p$set(1, list("PRINT", list(), NULL))
    },
    # GOTO statement
    p_command_goto = function(doc='command : GOTO INTEGER', p) {
      p$set(1, list('GOTO', strtoi(p$get(3))))
    },
    p_command_goto_bad = function(doc='command : GOTO error', p) {
      p$set(1, "INVALID LINE NUMBER IN GOTO")
    },
    # IF-THEN statement
    p_command_if = function(doc='command : IF relexpr THEN INTEGER', p) {
      p$set(1, list('IF', p$get(3), strtoi(p$get(5))))
    },
    p_command_if_bad = function(doc='command : IF error THEN INTEGER', p) {
      p$set(1, "BAD RELATIONAL EXPRESSION")
    },
    p_command_if_bad2 = function(doc='command : IF relexpr THEN error', p) {
      p$set(1, "INVALID LINE NUMBER IN THEN")
    },
    # FOR statement
    p_command_for = function(doc='command : FOR ID EQUALS expr TO expr optstep', p) {
      p$set(1, list('FOR', p$get(3), p$get(5), p$get(7), p$get(8)))
    },
    p_command_for_bad_initial = function(doc='command : FOR ID EQUALS error TO expr optstep', p) {
      p$set(1, "BAD INITIAL VALUE IN FOR STATEMENT")
    },
    p_command_for_bad_final = function(doc='command : FOR ID EQUALS expr TO error optstep', p) {
      p$set(1, "BAD FINAL VALUE IN FOR STATEMENT")
    },
    p_command_for_bad_step = function(doc='command : FOR ID EQUALS expr TO expr STEP error', p) {
      p$set(1, "MALFORMED STEP IN FOR STATEMENT")
    },
    # Optional STEP qualifier on FOR statement
    p_optstep = function(doc='optstep : STEP expr
                                  | empty', p) {
      if(p$length() == 3) p$set(1, p$get(3))
      else                p$set(1, NULL)
    },
    # NEXT statement
    p_command_next = function(doc='command : NEXT ID', p) {
      p$set(1, list('NEXT', p$get(3)))
    },
    p_command_next_bad = function(doc='command : NEXT error', p) {
      p$set(1, "MALFORMED NEXT")
    },
    # END statement
    p_command_end = function(doc='command : END', p) {
      p$set(1, list('END'))
    },
    # REM statement
    p_command_rem = function(doc='command : REM', p) {
      p$set(1, list('REM', p$get(2)))
    },
    # STOP statement
    p_command_stop = function(doc='command : STOP', p) {
      p$set(1, list('STOP'))
    },
    # DEF statement
    p_command_def = function(doc='command : DEF ID LPAREN ID RPAREN EQUALS expr', p) {
      p$set(1, list('FUNC', p$get(3), p$get(5), p$get(8)))
    },
    p_command_def_bad_rhs = function(doc='command : DEF ID LPAREN ID RPAREN EQUALS error', p) {
      p$set(1, "BAD EXPRESSION IN DEF STATEMENT")
    },
    p_command_def_bad_arg = function(doc='command : DEF ID LPAREN error RPAREN EQUALS expr', p) {
      p$set(1, "BAD ARGUMENT IN DEF STATEMENT")
    },
    # GOSUB statement
    p_command_gosub = function(doc='command : GOSUB INTEGER', p) {
      p$set(1, list('GOSUB', strtoi(p$get(3))))
    },
    p_command_gosub_bad = function(doc='command : GOSUB error', p) {
      p$set(1, "INVALID LINE NUMBER IN GOSUB")
    },
    # RETURN statement
    p_command_return = function(doc='command : RETURN', p) {
      p$set(1, list('RETURN'))
    },
    # DIM statement
    p_command_dim = function(doc='command : DIM dimlist', p) {
      p$set(1, list('DIM', p$get(3)))
    },
    p_command_dim_bad = function(doc='command : DIM error', p) {
      p$set(1, "MALFORMED VARIABLE LIST IN DIM")
    },
    # List of variables supplied to DIM statement
    p_dimlist = function(doc='dimlist : dimlist COMMA dimitem
                                      | dimitem', p) {
      if(p$length() == 4) {
        p$set(1, p$get(2))
        p$set(1, append(p$get(1), p$get(4)))
      } else p$set(1, list(p$get(2)))
    },
    # DIM items
    p_dimitem_single = function(doc='dimitem : ID LPAREN INTEGER RPAREN', p) {
      p$set(1, list(p$get(2), eval(p$get(4)), 0))
    },
    p_dimitem_double = function(doc='dimitem : ID LPAREN INTEGER COMMA INTEGER RPAREN', p) {
      p$set(1, list(p$get(2), eval(p$get(4)), eval(p$get(6))))
    },
    # Arithmetic expressions
    p_expr_binary = function(doc='expr : expr PLUS expr
                                       | expr MINUS expr
                                       | expr TIMES expr
                                       | expr DIVIDE expr
                                       | expr POWER expr', p) {
      p$set(1, list('BINOP', p$get(3), p$get(2), p$get(4)))
    },
    p_expr_number = function(doc='expr : INTEGER
                                       | FLOAT', p) {
      p$set(1, list('NUM', as.numeric(p$get(2))))
    },
    p_expr_variable = function(doc='expr : variable', p) {
      p$set(1, list('VAR', p$get(2)))
    },
    p_expr_group = function(doc='expr : LPAREN expr RPAREN', p) {
      p$set(1, list('GROUP', p$get(3)))
    },
    p_expr_unary = function(doc='expr : MINUS expr %prec UMINUS', p) {
      p$set(1, list('UNARY', '-', p$get(3)))
    },
    # Relational expressions
    p_relexpr = function(doc='relexpr : expr LT expr
                                      | expr LE expr
                                      | expr GT expr
                                      | expr GE expr
                                      | expr EQUALS expr
                                      | expr NE expr', p) {
      p$set(1, list('RELOP', p$get(3), p$get(2), p$get(4)))
    },
    # Variables
    p_variable = function(doc='variable : ID
                                        | ID LPAREN expr RPAREN
                                        | ID LPAREN expr COMMA expr RPAREN', p) {
           if(p$length() == 2) p$set(1, list(p$get(2), NULL, NULL))
      else if(p$length() == 5) p$set(1, list(p$get(2), p$get(4), NULL))
      else                     p$set(1, list(p$get(2), p$get(4), p$get(6)))
    },
    # Builds a list of variable targets as a R list
    p_varlist = function(doc='varlist : varlist COMMA variable
                                      | variable', p) {
      if(p$length() > 2) {
        p$set(1, p$get(2))
        p$get(1) <- append(p$get(1), p$get(4))
      } else p$set(1, list(p$get(2)))
    },
    # Builds a list of numbers as a R list
    p_numlist = function(doc='numlist : numlist COMMA number
                                      | number', p) {
      if(p$length() > 2) {
        p$set(1, p$get(2))
        p$get(1) <- append(p$get(1), p$get(4))
      } else p$set(1, list(p$get(2)))
    },
    # A number. May be an integer or a float
    p_number = function(doc='number : INTEGER
                                    | FLOAT', p) {
      p$set(1, eval(p$get(2)))
    },
    # A signed number.
    p_number_signed = function(doc='number : MINUS INTEGER
                                           | MINUS FLOAT', p) {
      p$set(1, eval(paste("-", p$get(3), collapse="")))
    },
    # List of targets for a print statement
    # Returns a list of tuples (label,expr)
    p_plist = function(doc='plist : plist COMMA pitem
                                  | pitem', p) {
      if(p$length() > 3) {
        p$set(1, p$get(2))
        p$set(1, append(p$get(1), p$get(4)))
      } else p$set(1, list(p$get(2)))
    },
    p_item_string = function(doc='pitem : STRING', p) {
      p$set(1, list(substr(p$get(2), 2, nchar(p$get(2))-1), NULL))
    },
    p_item_string_expr = function(doc='pitem : STRING expr', p) {
      p$set(1, list(tail(head(p$get(2), -1), -1), p$get(3)))
    },
    p_item_expr = function(doc='pitem : expr', p) {
      p$set(1, list("", p$get(2)))
    },
    # Empty
    p_empty = function(doc='empty : ', p) {
    },
    # Catastrophic error handler
    p_error = function(p) {
      if(!is.null(p)) cat("SYNTAX ERROR AT EOF\n")
    }
  )
)

'%nin%' <- Negate('%in%')

BasicInterpreter <- R6::R6Class("BasicInterpreter",
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
      self$functions[['SIN']] <- function(z) sin(self$eval(z))
      self$functions[['COS']] <- function(z) cos(self$eval(z))
      self$functions[['TAN']] <- function(z) tan(self$val(z))
      self$functions[['ATN']] <- function(z) atan(self$eval(z))
      self$functions[['EXP']] <- function(z) exp(self$eval(z))
      self$functions[['ABS']] <- function(z) abs(self$eval(z))
      self$functions[['LOG']] <- function(z) log(self$eval(z))
      self$functions[['SQR']] <- function(z) sqrt(self$eval(z))
      self$functions[['INT']] <- function(z) as.integer(self$eval(z))
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
           if(etype == 'NUM')   return(expr[[2]])
      else if(etype == 'GROUP') return(self$eval(expr[[2]]))
      else if(etype == 'UNARY') {
        if(expr[[2]] == '-') return(-self$eval(expr[[3]]))
      } else if(etype == 'BINOP') {
             if(expr[[2]] == '+') return(self$eval(expr[[3]]) + self$eval(expr[[4]]))
        else if(expr[[2]] == '-') return(self$eval(expr[[3]]) - self$eval(expr[[4]]))
        else if(expr[[2]] == '*') return(self$eval(expr[[3]]) * self$eval(expr[[4]]))
        else if(expr[[2]] == '/') return(self$eval(expr[[3]]) / self$eval(expr[[4]]))
        else if(expr[[2]] == '^') return(abs(self$eval(expr[[3]])) ^ self$eval(expr[[4]]))
      } else if(etype == 'VAR') {
        var_dim1_dim2 <- expr[[2]]
        var  <- var_dim1_dim2[[1]]
        dim1 <- var_dim1_dim2[[2]]
        dim2 <- var_dim1_dim2[[3]]
        if(is.null(dim1) && is.null(dim2)) {
          if(var %in% names(self$vars)) return(self$vars[[var]])
          else {
            print(sprintf("UNDEFINED VARIABLE %s AT LINE %s", var, as.character(self$stat[[self$pc]])))
            stop()
          }
        }
        # May be a list lookup or a function evaluation
        if(!is.null(dim1) && is.null(dim2)) {
          if(var %in% names(self$functions)) {
            # A function
            return(self$functions[[var]](dim1))
          } else {
            # A list evaluation
            if(var %in% names(self$lists)) {
              dim1val <- self$eval(dim1)
              if(dim1val < 1 || dim1val > length(self$lists[[var]])) {
                print(sprintf("LIST INDEX OUT OF BOUNDS AT LINE %s", self$stat[[self$pc]]))
                stop()
              }
              return(self$lists[[var]][[dim1val - 1]])
            }
          }
        }
        if(!is.null(dim1) && !is.null(dim2)) {
          if(var %in% names(self$tables)) {
            dim1val <- self$eval(dim1)
            dim2val <- self$eval(dim2)
            if(dim1val < 1 || dim1val > length(self$tables[[var]]) || dim2val < 1 || dim2val > length(self$tables[[var]][[1]])) {
              print(sprintf("TABLE INDEX OUT OUT BOUNDS AT LINE %s",
                             self$stat[[self.pc]]))
              stop()
            }
            return(self$tables[[var]][[dim1val - 1]][[dim2val - 1]])
          }
        }
      
        print(sprintf("UNDEFINED VARIABLE %s AT LINE %s", var, as.character(self$stat[[self$pc]])))
        stop()
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
        self$lists[var][dim1val - 1] = self$eval(value)
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
        line  <- self$stat[[self$pc]]
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
#            if(nchar(out) > 0) out <- paste(out, sprintf('%5s'))
            out <- paste(out, label, sep='', collapse='')
            if(!is.null(val)) {
              if(label != "") out <- paste(out, " ", sep='')
              eval <- self$eval(val)
              out <- paste(out, toString(eval), sep='')
            }
          }
          cat(out)
          end <- toString(instr[[3]])
          if(!(end == ',' || end == ';')) cat("\n")
          if(end == ',') cat(" ")
          if(end == ';') cat(" ")
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
            if(is.null(stepval)) stepval <- list('NUM', 1)
            stepval <- self$eval(stepval)    # Evaluate step here
            self$loops[[length(self$loops)+1]] <- list(self$pc, stepval)
          } else {
            # It's a repeat of the previous loop
            # Update the value of the loop variable according to the
            # step
            stepval  <- list('NUM', tail(self$loops, 1)[[1]][[2]])
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
          loopinst <- self$prog[[as.character(self$stat[[self$pc]])]]
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

lexer  <- rly::lex(Lexer)
parser <- rly::yacc(Parser)

library(testthat)
context("basic")

test_that("hello", {
  parser$restart()
  fileName <- 'BASIC/hello.bas'
  data <- readChar(fileName, file.info(fileName)$size)
  prog <- parser$parse(data, lexer)
  b <- BasicInterpreter$new(prog)
  expect_output(b$run(), "HELLO WORLD")
})

test_that("rand", {
  parser$restart()
  fileName <- 'BASIC/rand.bas'
  data <- readChar(fileName, file.info(fileName)$size)
  prog <- parser$parse(data, lexer)
  b <- BasicInterpreter$new(prog)
#  b$run()
#  expect_output(b$run(), "HELLO WORLD")
})