#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Infinite recursion")

Parser <- R6Class("Parser",
  public = list(
    tokens = c('NAME','NUMBER', 'PLUS','MINUS','TIMES','DIVIDE','EQUALS', 'LPAREN','RPAREN'),
    # Parsing rules
    precedence = list(c('left','PLUS','MINUS'),
                      c('left','TIMES','DIVIDE'),
                      c('right','UMINUS')),
    # dictionary of names
    names = new.env(hash=TRUE),
    p_statement_assign = function(doc='statement : NAME EQUALS expression', p) {
      self$names[[as.character(p$get(2))]] <- p$get(4)
    },
    p_statement_expr = function(doc='statement : expression', p) {
      cat(p$get(2))
      cat('\n')
    },
    p_expression_binop = function(doc='expression : expression PLUS expression
                                                  | expression MINUS expression
                                                  | expression TIMES expression
                                                  | expression DIVIDE expression', p) {
      if(p$get(3) == 'PLUS') p$set(1, p$get(2) + p$get(4))
      else if(p$get(3) == 'MINUS') p$set(1, p$get(2) - p$get(4))
      else if(p$get(3) == 'TIMES') p$set(1, p$get(2) * p$get(4))
      else if(p$get(3) == 'DIVIDE') p$set(1, p$get(2) / p$get(4))
    },
    p_expression_uminus = function(doc='expression : MINUS expression %prec UMINUS', p) {
      p$set(1, -p$get(3))
    },
    p_expression_group = function(doc='expression : LPAREN expression RPAREN', p) {
      p$set(1, p$get(3))
    },
    p_error = function(p) {
      cat(sprintf("Syntax error at '%s'", p$value))
    }
  )
)

test_that("recursion", {
  expect_output(expect_error(rly::yacc(Parser), "Unable to build parser"),
"WARN .* Token NUMBER defined, but not used
WARN .* There is 1 unused token
ERROR .* Infinite recursion detected for symbol expression
ERROR .* Infinite recursion detected for symbol statement")
})
