#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Panic mode recovery test")

Lexer <- R6::R6Class("Lexer",
  public = list(
    tokens = c('NAME','NUMBER', 
               'PLUS','MINUS','TIMES','DIVIDE','EQUALS', 
               'LPAREN','RPAREN'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_TIMES = '\\*',
    t_DIVIDE = '/',
    t_EQUALS = '=',
    t_LPAREN = '\\(',
    t_RPAREN = '\\)',
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
      cat(sprintf("Illegal character '%s'\n", t$value[[1]]))
      t$lexer$skip(1)
      return(t)
    }
  )
)

Parser <- R6::R6Class("Parser",
  public = list(
    tokens = Lexer$public_fields$tokens,
    # Parsing rules
    precedence = list(c('left','PLUS','MINUS'),
                      c('left','TIMES','DIVIDE'),
                      c('right','UMINUS')),
    p_statements = function(doc='statements : statements statement', p) {
    },
    p_statements_1 = function(doc='statements : statement', p) {
    },
    p_statement_assign = function(doc='statement : LPAREN NAME EQUALS expression RPAREN', p) {
      cat(sprintf("%s=%s\n", p$get(3), p$get(5)))
    },
    p_statement_expr = function(doc='statement : LPAREN expression RPAREN', p) {
      cat(p$get(2))
      cat('\n')
    },
    p_expression_binop = function(doc='expression : expression PLUS expression
                                                  | expression MINUS expression
                                                  | expression TIMES expression
                                                  | expression DIVIDE expression', p) {
      if(p$get(3) == '+') p$set(1, p$get(2) + p$get(4))
      else if(p$get(3) == '-') p$set(1, p$get(2) - p$get(4))
      else if(p$get(3) == '*') p$set(1, p$get(2) * p$get(4))
      else if(p$get(3) == '/') p$set(1, p$get(2) / p$get(4))
    },
    p_expression_uminus = function(doc='expression : MINUS expression %prec UMINUS', p) {
      p$set(1, -p$get(3))
    },
    p_expression_number = function(doc='expression : NUMBER', p) {
      p$set(1, p$get(2))
    },
    p_error = function(p) {
      if(!is.null(p)) cat(sprintf("Line %d: Syntax error at '%s'\n", p$lineno, p$value))
      
      # Scan ahead looking for a name token
       tok <- NULL
      while(TRUE) {
        tok <- p$lexer$token()
        if(is.null(tok) || tok$type == 'RPAREN') break
      }
      if(!is.null(tok)) p$parser$restart()
      return(NULL)
    }
  )
)

test_that("error", {
  lexer <- rly::lex(Lexer)
  parser <- rly::yacc(Parser)
  data <- "
(a = 3 + 4)
(b = 4 + * 5 - 6 + *)
(c = 10 + 11)
"
  expect_output(parser$parse(data, lexer), "a=7
Line 3: Syntax error at '\\*'
c=21")
})
