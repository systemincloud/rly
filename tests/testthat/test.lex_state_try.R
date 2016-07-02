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
    t_ignore = " \t",
    t_comment = function(re='/\\*', t) {
      cat('comment\n')
      t$lexer$begin('comment')
      return(NULL)
    },
    t_comment_body_part = function(re='(.|\\n)*\\*/',  t) {
      cat(sprintf("comment body %s\n", t$value))
      t$lexer$begin('INITIAL')
      return(NULL)
    },
    t_error = function(t) { }
  )
)
Lexer$set("public", "t_comment_error", Lexer$public_methods$t_error)
Lexer$set("public", "t_comment_ignore", Lexer$public_fields$t_ignore)

test_that("comment", {
  lexer = rly::lex(Lexer)
  lexer$input("3 + 4 /* This is a comment */ + 10")
  expect_equal(lexer$token()$value, "3")
  expect_equal(lexer$token()$value, "+")
  expect_equal(lexer$token()$value, "4")
  t <- NA
  expect_output(t <- lexer$token(), "comment\ncomment body * This is a comment */", fixed=TRUE)
  expect_equal(t$value, "+")
  expect_equal(lexer$token()$value, "10")
})
