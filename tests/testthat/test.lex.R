#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("basic lex")

getLexerCalc = function() {
  CalcLexer <- R6Class("CalcLexer",
    public = list(
      tokens = c('NAME','NUMBER'),
      literals = c('=','+','-','*','/', '(',')'),
      t_NAME = '[a-zA-Z_][a-zA-Z0-9_]*',
      t_NUMBER = function(re = '\\d+', t) {
        t$value = strtoi(t$value)
        return(t)
      },
      t_ignore = " \t",
      t_newline = function(re = '\n+', t) {
        t$lexer$lineno = t$lexer$lineno + t$value$count("\n")
      },
      t_error = function(t) {
        cat(sprintf("Illegal character '%s'", t$value[0]))
        t$lexer$skip(1)
      }
    )
  )
  return(CalcLexer$new())
}

test_that("str_length is number of characters", {
    lexer = getLexerCalc()
    lexer$input("5 + 3")
    expect_equal(lexer$token(), "5")
    expect_equal(lexer$token(), "+")
    expect_equal(lexer$token(), "3")
  }
)
