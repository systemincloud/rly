#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Attempt to define a rule with a bad-identifier name")

Parser1 <- R6Class("Parser1",
  public = list(
    tokens = c('NAME','NUMBER', 'PLUS','MINUS','TIMES','DIVIDE','EQUALS', 'LPAREN','RPAREN'),
    # Parsing rules
    precedence = list(c('left','PLUS','MINUS'),
                      c('left','TIMES','DIVIDE'),
                      c('right','UMINUS')),
    # dictionary of names
    names = new.env(hash=TRUE),
    p_statement_assign = function(doc='statement : NAME EQUALS expression', t) {
      names[t[1]] <- t[3]
    },
    p_statement_expr = function(doc='statement : expression', t) {
      cat(t[1])
    },
    p_statement_expr2 = function(doc='statement : bad&rule', t) { },
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

Parser2 <- R6Class("Parser2",
  public = list(
    tokens = c('NAME','NUMBER', 'PLUS','MINUS','TIMES','DIVIDE','EQUALS', 'LPAREN','RPAREN'),
    # Parsing rules
    precedence = list(c('left','PLUS','MINUS'),
      c('left','TIMES','DIVIDE'),
      c('right','UMINUS')),
    # dictionary of names
    names = new.env(hash=TRUE),
    p_statement_assign = function(doc='statement : NAME EQUALS expression', t) {
      names[t[1]] <- t[3]
    },
    p_statement_expr = function(doc='statement : expression', t) {
      cat(t[1])
    },
    p_badrule = function(doc='bad&rule : expression', t) { },
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


test_that("rule name", {
  expect_error(rly::yacc(Parser1), "ERROR> p_statement_expr2: Illegal name bad&rule in rule statement")
  expect_error(rly::yacc(Parser2), "ERROR> p_badrule: Illegal rule name bad&rule")
})
