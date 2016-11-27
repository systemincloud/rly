library(rly)

KEYWORDS = c('LET', 'READ', 'DATA', 'PRINT', 'GOTO', 'IF', 'THEN', 'FOR', 'NEXT', 'TO', 'STEP',
             'END', 'STOP', 'DEF', 'GOSUB', 'DIM', 'REM', 'RETURN', 'RUN', 'LIST', 'NEW')
TOKENS = c(KEYWORDS, c('EQUALS', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'POWER',
                       'LPAREN', 'RPAREN', 'LT', 'LE', 'GT', 'GE', 'NE',
                       'COMMA', 'SEMI', 'INTEGER', 'FLOAT', 'STRING',
                       'ID', 'NEWLINE'))

Lexer <- R6Class("Lexer",
  public = list(
    tokens = TOKENS,
    t_ignore = " \t",
    t_REM = function(re='REM .*', t) {
      return(t)
    },
    t_ID = function(re='[A-Z][A-Z0-9]*', t) {
      if(t$value %in% keywords) t$type <- t$value
      return(t)
    }
  ),
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
  t_newline = function(re='\\n+', t) {
    t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
    return(NULL)
  },
  t_error = function(t) {
    cat(sprintf("Illegal character '%s'", t$value[[1]]))
    t$lexer$skip(1)
    return(NULL)
  }
)

Parser <- R6Class("Parser",
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
      if(p$length() == 2 && p$get(2)) {
        p$set(1, new.env(hash=TRUE))
        line <- p$get(2)[[1]]
        stat <- p$get(2)[[2]]
        p$get(1)[[line]] <- stat
      } else if(p$length() == 3) {
        p$set(1, p$get(2))
        if(!p$get(1))  p$set(1, new.env(hash=TRUE))
        if(p$get(3)) {
          line <- p$get(2)[[1]]
          stat <- p$get(2)[[2]]
          p$get(1)[[line]] <- stat
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
    p_command_print_bad = function(doc='ccommand : PRINT error', p) {
      p$set(1, "MALFORMED PRINT STATEMENT")
    }
    # Optional ending on PRINT. Either a comma (,) or semicolon (;)

  )
)
