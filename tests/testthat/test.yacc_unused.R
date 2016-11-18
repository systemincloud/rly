#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("A grammar with an unused rule")

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
    p_expression_list = function(doc='exprlist : exprlist COMMA expression', t) {
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
        p_expression_list_2 = function(doc='exprlist : expression', t) {
        },
        p_error = function(t) {
          cat(sprintf("Syntax error at '%s'", t$value))
        }
    )
)

test_that("grammar", {
  expect_error(rly::yacc(Parser1), "ERROR .* Symbol COMMA used, but not defined as a token or a rule")
  expect_output(rly::yacc(Parser2), 
"WARN .* Rule exprlist defined, but not used 
WARN .* There is 1 unused rule 
WARN .* Symbol exprlist is unreachable")
})
