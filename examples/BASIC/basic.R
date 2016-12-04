#! /usr/bin/env Rscript

# An implementation of Dartmouth BASIC (1964)
#

source('basiclexparse.R')
source('basinterp.R')

# If a filename has been specified, we try to run it.
# If a runtime error occurs, we bail out and enter
# interactive mode below
args <- commandArgs(trailingOnly=TRUE)
b <- NA
if (length(args) == 2) {
} else b <- BasicInterpreter$new()

# Interactive mode.  This incrementally adds/deletes statements
# from the program stored in the BasicInterpreter object.  In
# addition, special commands 'NEW','LIST',and 'RUN' are added.
# Specifying a line number with no code deletes that line from
# the program.
