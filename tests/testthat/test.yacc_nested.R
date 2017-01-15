#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Nested")

Lexer <- R6::R6Class("Lexer",
  public = list(
    tokens = c('A','B', 'C'),
    t_A = 'A',
    t_B = 'B',
    t_C = 'C',
    t_error = function(t) {
    }
  )
)

Parser <- R6::R6Class("Parser",
  public = list(
    tokens = Lexer$public_fields$tokens,
    p_start = function(doc='start : A nest C', p) {
    },
    p_nest = function(doc='nest : B', p) {
      cat(p$get(-1))
      cat("\n")
    },
    p_error = function(p) {
    }
  )
)

test_that("nested", {
  lexer  <- rly::lex(Lexer)
  parser <- rly::yacc(Parser)

  expect_output(parser$parse('ABC', lexer), 'A')
  expect_output(parser$parse('ABC', lexer, tracking=TRUE), 'A')
  expect_output(parser$parse('ABC', lexer, tracking=TRUE, debug=TRUE),
"INFO \\[.*\\] RLY: PARSE DEBUG START
INFO \\[.*\\] 
INFO \\[.*\\] State  : 1
INFO \\[.*\\] Stack  :  \\. LexToken\\(A,A,1,1\\)
INFO \\[.*\\] Action : Shift and goto state 2
INFO \\[.*\\] 
INFO \\[.*\\] State  : 2
INFO \\[.*\\] Stack  : A \\. LexToken\\(B,B,1,2\\)
INFO \\[.*\\] Action : Shift and goto state 4
INFO \\[.*\\] 
INFO \\[.*\\] State  : 4
INFO \\[.*\\] Defaulted state 4: Reduce using 2
INFO \\[.*\\] Stack  : A B \\. NULL
INFO \\[.*\\] Action : Reduce rule \\[nest -> B\\] with \\[B\\] and goto state 5
A
INFO \\[.*\\] Result : <NULL> \\(\\)
INFO \\[.*\\] 
INFO \\[.*\\] State  : 5
INFO \\[.*\\] Stack  : A nest \\. LexToken\\(C,C,1,3\\)
INFO \\[.*\\] Action : Shift and goto state 6
INFO \\[.*\\] 
INFO \\[.*\\] State  : 6
INFO \\[.*\\] Defaulted state 6: Reduce using 1
INFO \\[.*\\] Stack  : A nest C \\. NULL
INFO \\[.*\\] Action : Reduce rule \\[start -> A nest C\\] with \\[A, , C\\] and goto state 3
INFO \\[.*\\] Result : <NULL> \\(\\)
INFO \\[.*\\] 
INFO \\[.*\\] State  : 3
INFO \\[.*\\] Stack  : start \\. \\$end
INFO \\[.*\\] Done   : Returning <NULL> \\(\\)
INFO \\[.*\\] PLY: PARSE DEBUG END")
 })