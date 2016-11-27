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
    p_command_print_bad = function(doc='command : PRINT error', p) {
      p$set(1, "MALFORMED PRINT STATEMENT")
    },
    # Optional ending on PRINT. Either a comma (,) or semicolon (;)
    p_optend = function(doc='optend : COMMA 
                                    | SEMI
                                    |', p) {
      if(p$lenght() == 2) p$set(1, p$get(2))
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
      p$set(1, list('NUM', eval(p$get(2))))
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
    }
    # Variables

    # Builds a list of variable targets as a R list

    # Builds a list of numbers as a R list

    # A number. May be an integer or a float

    # A signed number.

    # List of targets for a print statement
    # Returns a list of tuples (label,expr)

    # Empty

    # Catastrophic error handler
  )
)
