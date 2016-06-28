#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Parsing of Fortran H Edit descriptions")

#' These tokens can't be easily tokenized because they are of the following
#' form:
#'
#'   nHc1...cn
#'
#' where n is a positive integer and c1 ... cn are characters.
#'
#' This example shows how to modify the state of the lexer to parse
#' such tokens

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('H_EDIT_DESCRIPTOR'),
    t_ignore = " \t\n",
    t_H_EDIT_DESCRIPTOR = function(re="\\d+H.*", t) {
      # This grabs all of the remaining text
      i <- gregexpr(pattern ='H', t$value)[[1]][1]
      n <- strtoi(substring(t$value, 1, i-1))
      # Adjust the tokenizing position
      t$lexer$lexpos <- t$lexer$lexpos - nchar(t$value) + (i+1+n)
      t$value <- substring(t$value, i+1, i+n)
      return(t)
    },
    t_error = function(t) {
      cat(sprintf("Illegal character '%s'", t$value[0]))
      t$lexer$skip(1)
    }
  )
)

test_that("H Edit", {
  lexer <- rly::lex(Lexer)
  lexer$input("3Habc 10Habcdefghij 2Hxy")
  expect_equal(lexer$token()$value, "abc")
  expect_equal(lexer$token()$value, "abcdefghij")
  expect_equal(lexer$token()$value, "xy")
})
