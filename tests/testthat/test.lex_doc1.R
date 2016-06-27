#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Missing documentation string")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS','MINUS'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = function(t) {
      return(t)
    },
    t_error = function(t) {
      return(t)
    }
  )
)

test_that("missing regex", {
  results <- capture.output(rly::lex(Lexer))[[1]]
  expect_equal(results, "DEBUG>  Rule 't_NUMBER' requires an argument ")
})