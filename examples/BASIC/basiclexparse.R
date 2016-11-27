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
  t_EQUALS = '=',
  t_PLUS = '\\+',
  t_MINUS = '-',
  t_TIMES = '\\*',
  t_POWER = '\\^',
  t_DIVIDE = '/',
  t_LPAREN = '\\(',
  t_RPAREN = '\\)',
  t_LT = '<',
  t_LE = '<=',
  t_GT = '>',
  t_GE = '>=',
  t_NE = '<>',
  t_COMMA = '\\,',
  t_SEMI = ';',
  t_INTEGER = '\\d+',
  t_FLOAT = '((\\d*\\.\\d+)(E[\\+-]?\\d+)?|([1-9]\\d*E[\\+-]?\\d+))',
  t_STRING = '\\".*?\\"',
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