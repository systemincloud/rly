#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Bad state declaration")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS','MINUS'),
    states = list(c('comment', 'exclusive'),
                  c('comment', 'exclusive')),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = '\\d+',
    t_ignore = " \t",
    t_comment = function(re='/\\*', t) {
      cat('comment\n')
      t$lexer$begin('Entering comment state')
    },
    t_comment_body_part = function(re='(.|\\n)*\\*/',  t) {
      cat(sprintf("comment body %s\n", t$value))
      t$lexer$begin('INITIAL')
    },
    t_error = function(t) { }
  )
)

test_that("comment", {
  expect_output(expect_error(rly::lex(Lexer), "Can't build lexer"),
  "ERROR .* State 'comment' already defined")
})
