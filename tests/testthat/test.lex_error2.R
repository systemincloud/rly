#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("t_error defined, but not function")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS','MINUS'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = '\\d+',
    t_error = "foo"
  )
)

test_that("t_error not function", {
  expect_error(rly::lex(Lexer), "ERROR> Rule 't_error' must be defined as a function\n")
})
