#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Bad token name")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS', '-', 'MINUS'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = '\\d+',
    t_ignore = " \t",
    t_error = function(t) { }
  )
)

test_that("comment", {
  expect_error(rly::lex(Lexer), "ERROR> Bad token name '-'",
               fixed=TRUE)
})
