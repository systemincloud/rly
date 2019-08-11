
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://travis-ci.org/systemincloud/rly.svg?branch=master)](https://travis-ci.org/systemincloud/rly)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/rly)](https://cran.r-project.org/package=rly)
[![](https://cranlogs.r-pkg.org/badges/rly)](https://cran.r-project.org/package=rly)
[![codecov](https://codecov.io/gh/systemincloud/rly/branch/master/graph/badge.svg)](https://codecov.io/gh/systemincloud/rly)
[![PayPal
donation](https://img.shields.io/badge/donation-PayPal-red.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=UR288FRQUSYQE&item_name=rly+-+R+Lex+and+Yacc&currency_code=USD&source=url)

# rly

Tools to Create Formal Language Parsers

## Introduction

{rly} is a 100% R implementation of the common parsing tools [`lex` and
`yacc`](http://dinosaur.compilertools.net/).

`lex` is a “lexical analyzer generator”. It’s core functionality is to
split up an input stream into more usable elements. You can think of it
as a tools to help identify the interesting components in a text file,
such as `->` or `%>%` in an R script.

`yacc` is “yet another compiler-compiler” and the main task for it is to
take the tokens `lex` provides and process them contextually.

Together, you can use them to:

  - define what tokens a given language/input stream will accept
  - define what R code should be executed as a language file/input
    stream (e.g. a program) is parsed.

This project is a R clone of [Ply](https://github.com/dabeaz/ply).

## Installation

``` r
devtools::install_github("systemincloud/rly")
```

## Usage

{rly} consists of two files : `lex.R` and `yacc.R`.

``` r
library(rly)
```

## Examples

The `inst/examples` directory contains several different examples.

### Finding Tokens

We can build a “lexer” to pull out URLs from text input. Sure, we could
just use `stringi::stri_match_all_regex()` for this particular example,
but the intent it to work with a known, straightforward domain to see
how “lexing” works:

``` r
# Define what tokens we accept. In this case, just URLs

TOKENS <- c("URL")

# Build our "lexing" rules.

URLexer <- R6::R6Class(
  
  classname = "Lexer",
  
  public = list(
    
    # tell it abt the tokens we accept
    
    tokens = TOKENS, 
    
    # we use the t_ prefix to identify that this is the 
    # "matcher" for the token and then give it the regular
    # expression that goes with this token. The URL
    # regex is a toy one that says to match http or https
    # strings until it hits a space. The `t` parameter
    # is the the full context of the token parser at the
    # time it gets to this token.
    #
    # here, we're just printing a message out and continuing
    # but we could do anything we (programmatically) want
    
    t_URL = function(re = 'http[s]*://[^[:space:]]+', t) {
      # Be verbose when we find a URL
      message("Found URL: ", t$value) 
      return(t)
    },
    
    # whenever a newline is encounterd we increment a line #
    # counter. this is useful when providing contextual errors
    
    t_newline = function(re='\\n+', t) {
      t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
      return(NULL)
    },
    
    # the goal of the lexer is to give us valid input 
    # but we can ignore errors if we're just looking for 
    # certain things (like URLs)
    
    t_error = function(t) {
      t$lexer$skip(1)
      return(t)
    }
  )
)

# Create out lexer

lexer  <- rly::lex(URLexer)

# Feed it some data

lexer$input(s = "
http://google.com     https://rstudio.org/
Not a URL  https://rud.is/b Another non-URL
https://r-project.org/



https://one.more.url/with/some/extra/bits.html


")

# We'll put found URLs here (rly inefficient)
found_urls <- character(0) 

# keep track of the # of invalid tokens
invalid_ct <- 0

# Now, we'll iterate through the tokens we were given

repeat {
  
  tok <- lexer$token() # get the next token
  
  if (is.null(tok)) break # no more tokens, done with lexing
  
  switch(
    tok$type,
    
    # Do this when we find a token identified as a `URL`
    URL = found_urls <- append(found_urls, tok$value),
    
    # Do this whenever we find an invalid token
    error = invalid_ct <- invalid_ct + 1
  )
  
}
#> Found URL: http://google.com
#> Found URL: https://rstudio.org/
#> Found URL: https://rud.is/b
#> Found URL: https://r-project.org/
#> Found URL: https://one.more.url/with/some/extra/bits.html

invalid_ct
#> [1] 32

found_urls
#> [1] "http://google.com"                             
#> [2] "https://rstudio.org/"                          
#> [3] "https://rud.is/b"                              
#> [4] "https://r-project.org/"                        
#> [5] "https://one.more.url/with/some/extra/bits.html"
```

### Calculator Example

Here is an example showing a {rly} implementation of a calculator with
variables.

``` r
library(rly)

TOKENS = c('NAME', 'NUMBER')
LITERALS = c('=','+','-','*','/', '(',')')

Lexer <- R6::R6Class("Lexer",
  public = list(
    tokens = TOKENS,
    literals = LITERALS,
    t_NAME = '[a-zA-Z_][a-zA-Z0-9_]*',
    t_NUMBER = function(re='\\d+', t) {
      t$value <- strtoi(t$value)
      return(t)
    },
    t_ignore = " \t",
    t_newline = function(re='\\n+', t) {
      t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
      return(NULL)
    },
    t_error = function(t) {
      cat(sprintf("Illegal character '%s'", t$value[1]))
      t$lexer$skip(1)
      return(t)
    }
  )
)

Parser <- R6::R6Class("Parser",
  public = list(
    tokens = TOKENS,
    literals = LITERALS,
    # Parsing rules
    precedence = list(c('left','+','-'),
                      c('left','*','/'),
                      c('right','UMINUS')),
    # dictionary of names
    names = new.env(hash=TRUE),
    p_statement_assign = function(doc='statement : NAME "=" expression', p) {
      self$names[[as.character(p$get(2))]] <- p$get(4)
    },
    p_statement_expr = function(doc='statement : expression', p) {
      cat(p$get(2))
      cat('\n')
    },
    p_expression_binop = function(doc="expression : expression '+' expression
                                                  | expression '-' expression
                                                  | expression '*' expression
                                                  | expression '/' expression", p) {
           if(p$get(3) == '+') p$set(1, p$get(2) + p$get(4))
      else if(p$get(3) == '-') p$set(1, p$get(2) - p$get(4))
      else if(p$get(3) == '*') p$set(1, p$get(2) * p$get(4))
      else if(p$get(3) == '/') p$set(1, p$get(2) / p$get(4))
    },
    p_expression_uminus = function(doc="expression : '-' expression %prec UMINUS", p) {
      p$set(1, -p$get(3))
    },
    p_expression_group = function(doc="expression : '(' expression ')'", p) {
      p$set(1, p$get(3))
    },
    p_expression_number = function(doc='expression : NUMBER', p) {
      p$set(1, p$get(2))
    },
    p_expression_name = function(doc='expression : NAME', p) {
      p$set(1, self$names[[as.character(p$get(2))]])
    },
    p_error = function(p) {
      if(is.null(p)) cat("Syntax error at EOF")
      else           cat(sprintf("Syntax error at '%s'", p$value))
    }
  )
)

lexer  <- rly::lex(Lexer)
parser <- rly::yacc(Parser)

while(TRUE) {
  cat('calc > ')
  s = readLines(file("stdin"), n=1)
  if(s == 'exit') break
  parser$parse(s, lexer)
}
```
