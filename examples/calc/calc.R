#! /usr/bin/env Rscript

library(rly)

TOKENS = c('NAME', 'NUMBER')
LITERALS = c('=','+','-','*','/', '(',')')

Lexer <- R6Class("Lexer",
  public = list(
    tokens = TOKENS,
    literals = LITERALS,
    t_NAME = '[a-zA-Z_][a-zA-Z0-9_]*',
    t_NUMBER = function(re='\\d+', t) {
      t$value <- strtoi(t$value)
      return(t)
    },
    t_ignore = " \t",
    t_newline = function(re='\\n+', t) {
      t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
      return(NULL)
    },
    t_error = function(t) {
      cat(sprintf("Illegal character '%s'", t$value[1]))
      t$lexer$skip(1)
      return(t)
    }
  )
)

Parser <- R6Class("Parser",
  public = list(
    tokens = TOKENS,
    literals = LITERALS,
    # Parsing rules
    precedence = list(c('left','+','-'),
                      c('left','*','/'),
                      c('right','UMINUS')),
    # dictionary of names
    names = new.env(hash=TRUE),
    p_statement_assign = function(doc="statement : NAME '=' expression", p) {
      names[[p[2]]] <- p[4]
    },
    p_statement_expr = function(doc='statement : expression', p) {
      cat(p[2])
      cat('\n')
    },
    p_expression_binop = function(doc="expression : expression '+' expression
                                                  | expression '-' expression
                                                  | expression '*' expression
                                                  | expression '/' expression", p) {
      if(p[3] == '+') p[1] <- p[2] + p[4]
      else if(p[3] == '-') p[1] <- p[2] - p[4]
      else if(p[3] == '*') p[1] <- p[2] * p[4]
      else if(p[3] == '/') p[1] <- p[2] / p[4]
    },
    p_expression_uminus = function(doc="expression : '-' expression %prec UMINUS", p) {
      p[1] <- -p[3]
    },
    p_expression_group = function(doc="expression : '(' expression ')'", p) {
      p[1] <- p[3]
    },
    p_expression_number = function(doc='expression : NUMBER', p) {
      p[1] <- p[2]
    },
    p_expression_name = function(doc='expression : NAME', p) {
      p[1] <- names[[t[2]]]
    },
    p_error = function(p) {
      if(is.null(p)) cat("Syntax error at EOF")
      else           cat(sprintf("Syntax error at '%s'", p$value))
    }
  )
)

lexer  <- rly::lex(Lexer)
parser <- rly::yacc(Parser)

while(TRUE) {
  cat('calc > ')
  s = readLines(file("stdin"), n=1)
  if(s == 'exit') break
#  parser$parse(s, lexer, debug=TRUE)
}
