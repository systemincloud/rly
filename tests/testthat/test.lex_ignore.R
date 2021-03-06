#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Improperly specific ignore declaration")

Lexer <- R6::R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS','MINUS'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = '\\d+',
    t_ignore = function(re=' \t', t) { },
    t_error = function(t) {}
  )
)

test_that("t_ignore improper", {
  expect_output(expect_error(rly::lex(Lexer), "Can't build lexer"),
  "ERROR .* Rule 't_ignore' must be defined as a string")
})
