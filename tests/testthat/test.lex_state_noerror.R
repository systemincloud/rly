#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Declaration of a state without error")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS','MINUS'),
    states = list(c('comment', 'exclusive')),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = '\\d+',
    t_comment = function(re='/\\*', t) {
      t$lexer$begin('comment')
    },
    t_comment_body_part = function(re='(.|\\n)*\\*/',  t) {
      t$lexer$begin('INITIAL')
    },
    t_error = function(t) { }
  )
)

test_that("no error rule for state", {
  expect_output(rly::lex(Lexer), paste("DEBUG>  No error rule is defined for exclusive state 'comment' \n",
                                       "DEBUG>  No ignore rule is defined for exclusive state 'comment' ", sep=""))
})

