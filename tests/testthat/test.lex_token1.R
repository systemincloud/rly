#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Tests for absence of tokens variable")

Lexer <- R6Class("Lexer",
  public = list(
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_NUMBER = '\\d+',
    t_ignore = " \t",
    t_error = function(t) { }
  )
)

test_that("comment", {
  expect_output(rly::lex(Lexer), "DEBUG>  No token list is defined ",
                fixed=TRUE)
})
