#! /usr/bin/env Rscript

library(R6)

CalcParser <- R6Class("CalcParser",
  public = list(
    precedence = c(c('left','+','-'),
                   c('left','*','/'),
                   c('right','UMINUS')),
    names = NA, # dictionary of names
    initialize = function() {
      self$names <- new.env(hash=TRUE)
    },
    p_statement_assign = function(p) {   # statement : NAME "=" expression
      self$names[p[1]] <- p[3]
    },
    p_statement_expr = function(p) {     # statement : expression
      print(p[1])
    },
    # expression : expression '+' expression
    #            | expression '-' expression
    #            | expression '*' expression
    #            | expression '/' expression
    p_expression_binop = function(p) {
      if(p[2] == '+') p[0] = p[1] + p[3]
      else if(p[2] == '-') p[0] = p[1] - p[3]
      else if(p[2] == '*') p[0] = p[1] * p[3]
      else if(p[2] == '/') p[0] = p[1] / p[3]
    },
    p_expression_uminus = function(p) {  # expression : '-' expression %prec UMINUS
      p[0] = -p[2]
    },
    p_expression_group = function(p) {   # expression : '(' expression ')'
      p[0] = p[2]
    },
    p_expression_number = function(p) {  # expression : NUMBER
      p[0] = p[1]
    },
    p_expression_name = function(p) {    # expression : NAME
      p[0] = self$names[p[1]]
    },
    p_error = function(p) {
      if(missing(p)) stop(sprintf("Syntax error at '%s'", p.value))
      else stop("Syntax error at EOF")
    }
  )
)

CalcLexer <- R6Class("CalcLexer",
  public = list(
    tokens = c('NAME','NUMBER'),
    literals = c('=','+','-','*','/', '(',')'),
    t_NAME = '[a-zA-Z_][a-zA-Z0-9_]*',
    t_NUMBER = function(re = '\\d+', t) {
      t$value = strtoi(t$value)
      return(t)
    },
    t_ignore = " \t",
    t_newline = function(re = '\n+', t) {
      t$lexer$lineno = t$lexer$lineno + t$value$count("\n")
    },
    t_error = function(t) {
      cat(sprintf("Illegal character '%s'", t$value[0]))
      t$lexer$skip(1)
    }
  )
)

lexer = rly::lex(CalcLexer$new())
parser = rly::yacc(CalcParser$new())

while(TRUE) {
  cat('calc > ')
  s = readLines(file("stdin"), n=1)
  if(s == 'exit') break
  parser$parse(s, lexer=lexer)
}
