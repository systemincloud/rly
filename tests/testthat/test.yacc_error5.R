#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Lineno and position tracking with error tokens")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NAME','NUMBER', 
               'PLUS','MINUS','TIMES','DIVIDE','EQUALS', 
               'LPAREN','RPAREN'),
    t_PLUS   = '\\+',
    t_MINUS  = '-',
    t_TIMES  = '\\*',
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

Parser <- R6Class("Parser",
  public = list(
    tokens = Lexer$public_fields$tokens,
    # Parsing rules
    precedence = list(c('left','PLUS','MINUS'),
                      c('left','TIMES','DIVIDE'),
                      c('right','UMINUS')),
    # dictionary of names
    names = new.env(hash=TRUE),
    p_statement_assign = function(doc='statement : NAME EQUALS expression', p) {
      self$names[[as.character(p$get(2))]] <- p$get(4)
    },
    p_statement_assign_error = function(doc='statement : NAME EQUALS error', p) {
      line_start <- p$linespan(4)[[1]]
      line_end   <- p$linespan(4)[[2]]
      pos_start  <- p$lexspan(4)[[1]]
      pos_end    <- p$lexspan(4)[[2]]
      
      cat(sprintf("Assignment Error at %d:%d to %d:%d\n", line_start, pos_start, line_end, pos_end))
    },
    p_statement_expr = function(doc='statement : expression', p) {
      cat(p$get(2))
      cat("\n")
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
    p_expression_group = function(doc='expression : LPAREN expression RPAREN', p) {
      line_start <- p$linespan(3)[[1]]
      line_end   <- p$linespan(3)[[2]]
      pos_start  <- p$lexspan(3)[[1]]
      pos_end    <- p$lexspan(3)[[2]]
      cat(sprintf("Group at %d:%d to %d:%d\n", line_start, pos_start, line_end, pos_end))
      p$set(1, p$get(3))
    },
    p_expression_group_error = function(doc='expression : LPAREN error RPAREN', p) {
      line_start <- p$linespan(3)[[1]]
      line_end   <- p$linespan(3)[[2]]
      pos_start  <- p$lexspan(3)[[1]]
      pos_end    <- p$lexspan(3)[[2]]
      cat(sprintf("Syntax error at %d:%d to %d:%d\n", line_start, pos_start, line_end, pos_end))
      p$set(1, 0)
    },
    p_expression_number = function(doc='expression : NUMBER', p) {
      p$set(1, p$get(2))
    },
    p_expression_name = function(doc='expression : NAME', p) {
      p$set(1, self$names[[as.character(p$get(2))]])
    },
    p_error = function(p) {
      cat(sprintf("Syntax error at '%s'\n", p$value))
    }
  )
)

test_that("error", {
  lexer <- rly::lex(Lexer)
  parser <- rly::yacc(Parser)
  data <- "
a = 3 +
(4*5) +
(a b c) +
+ 6 + 7
"
  expect_output(parser$parse(data, lexer, tracking=TRUE),
"Group at 3:11 to 3:13
Syntax error at 'b'
Syntax error at 4:19 to 4:23
Assignment Error at 2:6 to 5:28
13")
})
