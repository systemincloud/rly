#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Missing t_error() rule")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS','MINUS'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = '\\d+'
  )
)

test_that("no t_error()", {
  expect_output(rly::lex(Lexer), "WARN> No t_error rule is defined")
})
