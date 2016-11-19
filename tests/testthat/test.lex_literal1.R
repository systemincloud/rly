#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Bad literal specification")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER'),
    literals = c("+","-","**"),
    t_NUMBER = function(re='\\d+', t) {
      return(t)
    },
    t_error = function(t) {}
  )
)

test_that("literal not single", {
  expect_error(rly::lex(Lexer), "ERROR> Invalid literal. Must be a single character")
})
