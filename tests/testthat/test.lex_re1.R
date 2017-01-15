#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Bad regular expression in a string")

Lexer <- R6::R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS','MINUS'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = '(\\d+',
    t_error = function(t) { }
  )
)

test_that("wrong regex", {
  expect_output(expect_error(rly::lex(Lexer), "Can't build lexer"),
  "ERROR .* Invalid regular expression for rule 't_NUMBER'")
})