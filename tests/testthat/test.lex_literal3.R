#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("An empty literal specification given as a list")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER'),
    literals = c(),
    t_NUMBER = function(re='\\d+', t) {
      return(t)
    },
    t_error = function(t) {}
  )
)

test_that("literals empty", {
  rly::lex(Lexer)
})
