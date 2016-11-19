#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("t_error defined as function, but with wrong # args")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS','MINUS'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = '\\d+',
    t_error = function() {}
  )
)

test_that("t_error missing arg", {
  expect_error(rly::lex(Lexer), "ERROR> Rule error requires an argument")
})
