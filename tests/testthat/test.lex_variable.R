#! /usr/bin/env Rscript

library(testthat)
library(rly)

library(testthat)
library(rly)

context("re as variable")

re_PLUS <- '\\+'
re_MINUS <- '-'
re_TIMES <- '\\*'
re_DIVIDE <- '/'
re_EQUALS <- '='
re_LPAREN <- '\\('
re_RPAREN <- '\\)'
re_NAME <- '[a-zA-Z_][a-zA-Z0-9_]*'
re_NUMBER <- '\\d+'
re_ignore <- " \t"
re_newline <- '\\n+'

Lexer <- R6::R6Class("Lexer",
  public = list(
    msg = NA,

    tokens = c('NAME','NUMBER', 'PLUS','MINUS','TIMES','DIVIDE','EQUALS', 'LPAREN','RPAREN'),
    t_PLUS = re_PLUS,
    t_MINUS = re_MINUS,
    t_TIMES = re_TIMES,
    t_DIVIDE = re_DIVIDE,
    t_EQUALS = re_EQUALS,
    t_LPAREN = re_LPAREN,
    t_RPAREN = re_RPAREN,
    t_NAME = re_NAME,
    t_NUMBER = function(re = re_NUMBER, t) {
      t$value <- strtoi(t$value)
      return(t)
    },
    t_ignore = re_ignore,
    t_newline = function(re = re_newline, t) {
      t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
      return(NULL)
    },
    t_error = function(t) {
      self$msg <- sprintf("Illegal character '%s'", t$value[1])
      t$lexer$skip(1)
      return(t)
    }
  )
)

test_that("calclex: +", {
  lexer <- rly::lex(Lexer)
  lexer$input("5 + 3")
  expect_equal(lexer$token()$value, 5)
  expect_equal(lexer$token()$value, "+")
  expect_equal(lexer$token()$value, 3)
})

test_that("calclex: *", {
  lexer <- rly::lex(Lexer)
  lexer$input("x * y")
  expect_equal(lexer$token()$value, "x")
  expect_equal(lexer$token()$value, "*")
  expect_equal(lexer$token()$value, "y")
})

test_that("calclex: without spaces", {
  lexer <- rly::lex(Lexer)
  lexer$input("(x+y)*4")
  expect_equal(lexer$token()$value, "(")
  expect_equal(lexer$token()$value, "x")
  expect_equal(lexer$token()$value, "+")
  expect_equal(lexer$token()$value, "y")
  expect_equal(lexer$token()$value, ")")
  expect_equal(lexer$token()$value, "*")
  expect_equal(lexer$token()$value, 4)
})

test_that("calclex: new lines", {
  lexer <- rly::lex(Lexer)

  lexer$input("x\n\ny")
  expect_equal(lexer$token()$value, "x")
  expect_equal(lexer$token()$lineno, 3)

  lexer$input("x

  y")
  expect_equal(lexer$token()$value, "x")
  expect_equal(lexer$token()$lineno, 5)
})

test_that("calclex: error", {
  lexer <- rly::lex(Lexer)

  lexer$input("x?y")
  expect_equal(lexer$token()$value, "x")
  lexer$token()
  expect_equal(lexer$instance$msg, "Illegal character '?'")
  expect_equal(lexer$token()$value, "y")
})
