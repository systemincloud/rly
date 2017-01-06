#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("precedence")

Lexer1 <- R6Class("Lexer1",
  public = list(
    tokens = c('THEME','LAYER'),
    literals = c(),
    t_THEME = '\\s(\\+|\\|)\\stheme',
    t_LAYER = '\\s(\\+|\\|)\\s[a-z_]+',
    t_error = function(t) { }
  )
)

Lexer2 <- R6Class("Lexer2",
  public = list(
    tokens = c('THEME','LAYER'),
    literals = c(),
    t_LAYER = '\\s(\\+|\\|)\\s[a-z_]+',
    t_THEME = '\\s(\\+|\\|)\\stheme',
    t_error = function(t) { }
  )
)

Lexer3 <- R6Class("Lexer3",
  public = list(
    tokens = c('THEME','LAYER'),
    literals = c(),
    t_THEME = '\\s(\\+|\\|)\\stheme',
    t_LAYER = function(re='\\s(\\+|\\|)\\s[a-z_]+', t) { return(t) },
    t_error = function(t) { }
  )
)

Lexer4 <- R6Class("Lexer4",
  public = list(
    tokens = c('THEME','LAYER'),
    literals = c(),
    t_LAYER = function(re='\\s(\\+|\\|)\\s[a-z_]+', t) { return(t) },
    t_THEME = '\\s(\\+|\\|)\\stheme',
    t_error = function(t) { }
  )
)

Lexer5 <- R6Class("Lexer5",
  public = list(
    tokens = c('THEME','LAYER'),
    literals = c(),
    t_THEME = function(re='\\s(\\+|\\|)\\stheme', t) { return(t) },
    t_LAYER = function(re='\\s(\\+|\\|)\\s[a-z_]+', t) { return(t) },
    t_error = function(t) { }
  )
)

Lexer6 <- R6Class("Lexer6",
  public = list(
    tokens = c('THEME','LAYER'),
    literals = c(),
    t_LAYER = function(re='\\s(\\+|\\|)\\s[a-z_]+', t) { return(t) },
    t_THEME = function(re='\\s(\\+|\\|)\\stheme', t) { return(t) },
    t_error = function(t) { }
  )
)

test_that("1", {
  lexer <- rly::lex(Lexer1)
  lexer$input(" + theme")
  expect_equal(lexer$token()$type, 'THEME')
  lexer$input(" + myvar1")
  expect_equal(lexer$token()$type, 'LAYER')
})

test_that("2", {
  lexer <- rly::lex(Lexer2)
  lexer$input(" + theme")
  expect_equal(lexer$token()$type, 'LAYER')
  lexer$input(" + myvar1")
  expect_equal(lexer$token()$type, 'LAYER')
})

test_that("3", {
  lexer <- rly::lex(Lexer3)
  lexer$input(" + theme")
  expect_equal(lexer$token()$type, 'LAYER')
  lexer$input(" + myvar1")
  expect_equal(lexer$token()$type, 'LAYER')
})

test_that("4", {
  lexer <- rly::lex(Lexer4)
  lexer$input(" + theme")
  expect_equal(lexer$token()$type, 'LAYER')
  lexer$input(" + myvar1")
  expect_equal(lexer$token()$type, 'LAYER')
})

test_that("5", {
  lexer <- rly::lex(Lexer5)
  lexer$input(" + theme")
  expect_equal(lexer$token()$type, 'THEME')
  lexer$input(" + myvar1")
  expect_equal(lexer$token()$type, 'LAYER')
})

test_that("6", {
  lexer <- rly::lex(Lexer6)
  lexer$input(" + theme")
  expect_equal(lexer$token()$type, 'LAYER')
  lexer$input(" + myvar1")
  expect_equal(lexer$token()$type, 'LAYER')
})