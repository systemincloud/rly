#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Terminal used on the left-hand-side")

Parser <- R6Class("Parser",
  public = list(
    tokens = c('NAME','NUMBER', 'PLUS','MINUS','TIMES','DIVIDE','EQUALS', 'LPAREN','RPAREN'),
    # Parsing rules
    precedence = list(c('left','PLUS','MINUS'),
                      c('left','TIMES','DIVIDE'),
                      c('right','UMINUS')),
    # dictionary of names
    names = new.env(hash=TRUE),
    p_statement_assign = function(doc='NUMBER : NAME EQUALS expression', t) {
      names[t[1]] <- t[3]
    },
    p_statement_expr = function(doc='statement : expression', t) {
      cat(t[1])
    },
    p_expression_binop = function(doc='expression : expression PLUS expression
                                                  | expression MINUS expression
                                                  | expression TIMES expression
                                                  | expression DIVIDE expression', t) {
      if     (t[2] == '+') t[0] <- t[1] + t[3]
      else if(t[2] == '-') t[0] <- t[1] - t[3]
      else if(t[2] == '*') t[0] <- t[1] * t[3]
      else if(t[2] == '/') t[0] <- t[1] / t[3]
    },
    p_expression_uminus = function(doc='expression : MINUS expression %prec UMINUS', t) {
      t[0] <- -t[2]
    },
    p_expression_group = function(doc='expression : LPAREN expression RPAREN', t) {
      t[0] <- t[2]
    },
    p_expression_number = function(doc='expression : NUMBER', t) {
      t[0] <- t[1]
    },
    p_expression_name = function(doc='expression : NAME', t) {
      t[0] <- names[t[1]]
    },
    p_error = function(t) {
      cat(sprintf("Syntax error at '%s'", t$value))
    }
  )
)

test_that("terminal", {
  expect_error(rly::yacc(Parser), "ERROR> p_statement_assign: Illegal rule name NUMBER. Already defined as a token", fixed=TRUE)
})
