#! /usr/bin/env Rscript

# An implementation of Dartmouth BASIC (1964)
#
library(rly)

source('basiclexparse.R')
source('basinterp.R')

lexer  <- rly::lex(Lexer)
parser <- rly::yacc(Parser)

# If a filename has been specified, we try to run it.
# If a runtime error occurs, we bail out and enter
# interactive mode below
args <- commandArgs(trailingOnly=TRUE)
b <- NA
if (length(args) == 1) {
  fileName <- args[[1]]
  data <- readChar(fileName, file.info(fileName)$size)
  prog <- parser$parse(data, lexer, debug=TRUE)
  b <- BasicInterpreter$new(prog)
  b$run()
} else b <- BasicInterpreter$new()

# Interactive mode.  This incrementally adds/deletes statements
# from the program stored in the BasicInterpreter object.  In
# addition, special commands 'NEW','LIST',and 'RUN' are added.
# Specifying a line number with no code deletes that line from
# the program.
while(TRUE) {
  cat('[BASIC] ')
  line <- readLines(file("stdin"), n=1)
  prog <-parser$parse(line, lexer)
  if(is.null(prog)) next
  
  b$add_statements(prog)
  
}
