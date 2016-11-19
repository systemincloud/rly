#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Return a bad token name")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS', 'MINUS'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = function(re='\\d+', t) {
      t$type = "NUM"
      return(t)
    },
    t_ignore = " \t",
    t_error = function(t) {
      cat("XXXX")
      return(t)
    }
  )
)

test_that("bad token type", {
  lexer <- rly::lex(Lexer)
  lexer$input("1234")
  expect_error(lexer$token(), "Rule 't_NUMBER' returned an unknown token type 'NUM'")
})
