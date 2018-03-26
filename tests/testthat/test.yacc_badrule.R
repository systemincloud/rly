#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Syntax problems in the rule strings")

Parser1 <- R6::R6Class("Parser1",
  public = list(
    tokens = c('NAME','NUMBER', 'PLUS','MINUS','TIMES','DIVIDE','EQUALS', 'LPAREN','RPAREN'),
    # Parsing rules
    precedence = list(c('left','PLUS','MINUS'),
                      c('left','TIMES','DIVIDE','MINUS'),
                      c('right','UMINUS')),
    # dictionary of names
    names = new.env(hash=TRUE),
    p_statement_assign = function(doc='statement NAME EQUALS expression', p) {
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
    p_expression_number = function(doc='expression : NUMBER', p) {
      p$set(1, p$get(2))
    },
    p_expression_name = function(doc='expression : NAME', p) {
      t[0] <- names[t[1]]
    },
    p_error = function(p) {
      cat(sprintf("Syntax error at '%s'", p$value))
    }
  )
)

Parser2 <- R6::R6Class("Parser2",
  public = list(
    tokens = c('NAME','NUMBER', 'PLUS','MINUS','TIMES','DIVIDE','EQUALS', 'LPAREN','RPAREN'),
    # Parsing rules
    precedence = list(c('left','PLUS','MINUS'),
                      c('left','TIMES','DIVIDE','MINUS'),
                      c('right','UMINUS')),
    # dictionary of names
    names = new.env(hash=TRUE),
    p_statement_assign = function(doc='statement : NAME EQUALS expression', p) {
      self$names[[as.character(p$get(2))]] <- p$get(4)
    },
    p_statement_expr = function(doc='statement', p) {
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
    p_expression_number = function(doc='expression : NUMBER', p) {
      p$set(1, p$get(2))
    },
    p_expression_name = function(doc='expression : NAME', p) {
      p$set(1, self$names[[as.character(p$get(2))]])
    },
    p_error = function(p) {
      cat(sprintf("Syntax error at '%s'", p$value))
    }
  )
)

Parser3 <- R6::R6Class("Parser3",
  public = list(
    tokens = c('NAME','NUMBER', 'PLUS','MINUS','TIMES','DIVIDE','EQUALS', 'LPAREN','RPAREN'),
    # Parsing rules
    precedence = list(c('left','PLUS','MINUS'),
                      c('left','TIMES','DIVIDE','MINUS'),
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
                                       expression MINUS expression
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
    p_expression_number = function(doc='expression : NUMBER', p) {
      p$set(1, p$get(2))
    },
    p_expression_name = function(doc='expression : NAME', p) {
      p$set(1, self$names[[as.character(p$get(2))]])
    },
    p_error = function(p) {
      cat(sprintf("Syntax error at '%s'", p$value))
    }
  )
)

Parser4 <- R6::R6Class("Parser4",
  public = list(
    tokens = c('NAME','NUMBER', 'PLUS','MINUS','TIMES','DIVIDE','EQUALS', 'LPAREN','RPAREN'),
    # Parsing rules
    precedence = list(c('left','PLUS','MINUS'),
                      c('left','TIMES','DIVIDE','MINUS'),
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
    p_expression_uminus = function(doc='expression: MINUS expression %prec UMINUS', p) {
      p$set(1, -p$get(3))
    },
    p_expression_group = function(doc='expression : LPAREN expression RPAREN', p) {
      p$set(1, p$get(3))
    },
    p_expression_number = function(doc='expression : NUMBER', p) {
      p$set(1, p$get(2))
    },
    p_expression_name = function(doc='expression : NAME', p) {
      p$set(1, self$names[[as.character(p$get(2))]])
    },
    p_error = function(p) {
      cat(sprintf("Syntax error at '%s'", p$value))
    }
  )
)

test_that("rule", {
  expect_output(expect_error(rly::yacc(Parser1), "\\[YaccError\\]Unable to build parser"),
  "ERROR .* \\[SyntaxError\\]p_statement_assign: Syntax error. Expected ':'")
  expect_output(expect_error(rly::yacc(Parser2), "\\[YaccError\\]Unable to build parser"),
  "ERROR .* \\[SyntaxError\\]p_statement_expr: Syntax error in rule statement")
  expect_output(expect_error(rly::yacc(Parser3), "\\[YaccError\\]Unable to build parser"),
  "ERROR .* \\[SyntaxError\\]p_expression_binop: Syntax error. Expected ':'")
  expect_output(expect_error(rly::yacc(Parser4), "\\[YaccError\\]Unable to build parser"),
  "ERROR .* \\[SyntaxError\\]p_expression_uminus: Syntax error. Expected ':'")
})
