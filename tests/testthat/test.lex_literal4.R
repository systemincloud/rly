#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Literals as a string")

Lexer <- R6::R6Class("Lexer",
  public = list(
    tokens = c('NUMBER'),
    literals = "+-*",
    t_NUMBER = function(re='\\d+', t) {
      return(t)
    },
    t_error = function(t) {}
  )
)

test_that("literals as a string", {
  expect_output(rly::lex(Lexer), NA)
})
