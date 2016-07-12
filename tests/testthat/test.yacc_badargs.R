#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Rules with wrong # args")

Parser <- R6Class("Parser",
  public = list(
    # Parsing rules
    precedence = list(c('left','PLUS','MINUS'),
                      c('left','TIMES','DIVIDE'),
                      c('right','UMINUS')),
    # dictionary of names
    names = new.env(hash=TRUE),
    p_statement_assign = function(re='statement : NAME EQUALS expression', t, s) {
      names[t[1]] <- t[3]
    },
    p_statement_expr = function(re='statement : expression') {
      cat(t[1])
    },
    p_expression_binop = function(re='expression : expression PLUS expression
                                                 | expression MINUS expression
                                                 | expression TIMES expression
                                                 | expression DIVIDE expression', t) {
      if(t[2] == '+')      t[0] <- t[1] + t[3]
      else if(t[2] == '-') t[0] <- t[1] - t[3]
      else if(t[2] == '*') t[0] <- t[1] * t[3]
      else if(t[2] == '/') t[0] <- t[1] / t[3]
    },
    p_expression_uminus = function(re='expression : MINUS expression %prec UMINUS', t) {
      t[0] <- -t[2]
    },
    p_expression_group = function(re='expression : LPAREN expression RPAREN', t) {
      t[0] <- t[2]
    },
    p_expression_number = function(re='expression : NUMBER', t) {
      t[0] <- t[1]
    },
    p_expression_name = function(re='expression : NAME', t) {
      t[0] <- names[t[1]]
    },
    p_error = function(t) {
      cat(sprintf("Syntax error at '%s'", t$value))
    }
  )
)

test_that("missing regex", {
  rly::yacc(Parser)
#  expect_output(rly::yacc(Parser), "ERROR>  Rule 't_NUMBER' requires an argument ")
})
