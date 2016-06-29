#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Test lex's ability to handle a large number of tokens")

Lexer <- R6Class("Lexer",
  lock_objects = FALSE,
  public = list(
    t_ignore = " \t",
    t_error = function(t) {}
  )
)

tokens <- paste0("TOK", 1:1000)

Lexer$set("public", "tokens", tokens)

for(t in tokens) {
  Lexer$set("public", sprintf("t_%s", t), sprintf('%s:', t))
}

test_that("many tokens", {
  lexer <- rly::lex(Lexer)
  lexer$input("TOK34: TOK143: TOK269: TOK372: TOK452: TOK561: TOK999:")
  expect_equal(lexer$token()$value, "TOK34:")
  expect_equal(lexer$token()$value, "TOK143:")
  expect_equal(lexer$token()$value, "TOK269:")
  expect_equal(lexer$token()$value, "TOK372:")
  expect_equal(lexer$token()$value, "TOK452:")
  expect_equal(lexer$token()$value, "TOK561:")
  expect_equal(lexer$token()$value, "TOK999:")
})


#
#
#for tok in tokens:
#    if sys.version_info[0] < 3:
#        exec("t_%s = '%s:'" % (tok,tok))
#    else:
#        exec("t_%s = '%s:'" % (tok,tok), globals())
#
#t_ignore = " \t"
#
#def t_error(t):
#    pass
#
#lex.lex(optimize=1,lextab="manytab")
#lex.runmain(data="TOK34: TOK143: TOK269: TOK372: TOK452: TOK561: TOK999:")
#

