#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("ignore declaration as a raw string")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS','MINUS'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = '\\d+',
    t_ignore = ' \t',
    t_error = function(t) {}
  )
)

test_that("t_ignore improper", {
  rly::lex(Lexer)
})
