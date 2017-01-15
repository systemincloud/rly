#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("A grammar with reduce/reduce conflicts and a rule that never gets reduced")

Parser <- R6::R6Class("Parser",
  public = list(
    tokens = c('A', 'B', 'C'),
    p_grammar = function(doc='rule1 : rule2 B
                            | rule2 C

                              rule2 : rule3 B
                            | rule4
                            | rule5

                              rule3 : A

                              rule4 : A

                              rule5 : A', p) {
    }
  )
)

test_that("rr_unused", {
  expect_output(rly::yacc(Parser, debug=TRUE),
"WARN .* no p_error\\(\\) function is defined
Generating LALR tables 
WARN .* 3 reduce/reduce conflicts
WARN .* reduce/reduce conflict in state 2 resolved using rule \\(rule3 -> A\\)
WARN .* rejected rule \\(rule4 -> A\\) in state 2
WARN .* reduce/reduce conflict in state 2 resolved using rule \\(rule3 -> A\\)
WARN .* rejected rule \\(rule5 -> A\\) in state 2
WARN .* reduce/reduce conflict in state 2 resolved using rule \\(rule4 -> A\\)
WARN .* rejected rule \\(rule5 -> A\\) in state 2
WARN .* Rule \\(rule5 -> A\\) is never reduced")          
})
