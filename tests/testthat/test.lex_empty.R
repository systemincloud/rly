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
  expect_output(expect_error(rly::lex(Lexer), "Can't build lexer"),
  "ERROR .* No rules of the form t_rulename are defined")
})
