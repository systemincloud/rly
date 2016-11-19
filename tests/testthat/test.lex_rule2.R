#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Rule function with incorrect number of arguments")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS','MINUS'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = function(re='\\d+') { },
    t_error = function(t) { }
  )
)

test_that("incorect # args", {
  expect_error(rly::lex(Lexer), "ERROR> Rule 't_NUMBER' requires an argument")
})
