#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Regular expression rule matches empty string")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS','MINUS'),
    t_PLUS = '\\+?',
    t_MINUS = '-',
    t_NUMBER = '\\d+',
    t_error = function(t) { }
  )
)

test_that("regex matches empty string", {
  expect_output(rly::lex(Lexer), "ERROR>  Regular expression for rule 't_PLUS' matches empty string ")
})