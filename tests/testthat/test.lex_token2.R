#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Tests for tokens of wrong type")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = 'NUMBER PLUS MINUS',
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = '\\d+',
    t_ignore = " \t",
    t_error = function(t) { }
  )
)

test_that("comment", {
  expect_output(rly::lex(Lexer), "ERROR>  Bad token name 'NUMBER PLUS MINUS'",
                fixed=TRUE)
})
