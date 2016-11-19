#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Rule incorrect")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS','MINUS'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = 1,
    t_error = function(t) { }
  )
)

test_that("rule incorrect", {
  expect_output(expect_error(rly::lex(Lexer), "Can't build lexer"),
  "ERROR .* t_NUMBER not defined as a function or string")
})
