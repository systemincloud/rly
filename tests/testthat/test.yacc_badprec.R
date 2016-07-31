#! /usr/bin/env Rscript

library(testthat)
library(rly)

source("test.calclex.R")

context("Bad precedence specifier")

Parser <- R6Class("Parser",
  public = list(
    tokens = Lexer$public_fields$tokens,
    # Parsing rules
    precedence = "blah",
    # dictionary of names
    names = new.env(hash=TRUE),
    p_statement_assign = function(doc='statement : NAME EQUALS expression', t) {
      names[t[1]] <- t[3]
    },
    p_statement_expr = function(doc='statement : expression', t) {
      cat(t[1])
    },
    p_expression_binop = function(doc='expression : expression PLUS expression
        | expression MINUS expression
        | expression TIMES expression
        | expression DIVIDE expression', t) {
      if(t[2] == '+')      t[0] <- t[1] + t[3]
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

test_that("precedence", {
    expect_output(rly::yacc(Parser), "ERROR>  precedence must be a list")
})