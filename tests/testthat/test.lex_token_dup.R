#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Duplicate token name in tokens")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS','MINUS', 'NUMBER'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = function(re='\\d+', t) {
      return(t)
    },
    t_ignore = " \t",
    t_error = function(t) { }
  )
)

test_that("comment", {
  expect_output(rly::lex(Lexer), "WARN>  Token 'NUMBER' multiply defined ",
                fixed=TRUE)
})
