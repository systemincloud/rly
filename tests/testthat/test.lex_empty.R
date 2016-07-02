#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("No rules defined")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NUMBER', 'PLUS','MINUS')
  )
)

test_that("missing rules", {
  expect_output(rly::lex(Lexer), "ERROR>  No rules of the form t_rulename are defined ")
})
