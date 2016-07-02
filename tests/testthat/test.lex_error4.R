#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("t_error defined as function, but too many args")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS','MINUS'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = '\\d+',
    t_error = function(t, s) {}
  )
)

test_that("t_error too many arg", {
  expect_output(rly::lex(Lexer), "ERROR>  Rule error has too many arguments ")
})
