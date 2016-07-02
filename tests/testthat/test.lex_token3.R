#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("tokens is right type, but is missing a token for one rule")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = '\\d+',
    t_ignore = " \t",
    t_error = function(t) { }
  )
)

test_that("comment", {
  expect_output(rly::lex(Lexer), "ERROR>  Rule 't_MINUS' defined for an unspecified token MINUS ",
                fixed=TRUE)
})
