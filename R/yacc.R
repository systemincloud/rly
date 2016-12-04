# MIT License
#
# Copyright (c) 2016 System in Cloud - Marek Jagielski
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#    
#    The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# This work is derived from PLY python library:
# -----------------------------------------------------------------------------
# ply: lex.py
#
# Copyright (C) 2001-2016
# David M. Beazley (Dabeaz LLC)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the name of the David Beazley or Dabeaz LLC may be used to
#   endorse or promote products derived from this software without
#  specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------

'%nin%' <- Negate('%in%')

error_count <- 3    # Number of symbols that must be shifted to leave recovery mode

# Unique identificator of environment object
# 
# This function is retrieving an phisical address for an 
# environment object using R internal inspect function
# 
# @usage
# id(x)
# 
# @param x  environment
# @return The address of the object
# 
#' @useDynLib rly
id = function(x) .Call("id", x)

# Generate random string
# 
# This function generates a random string 
# of a given length only from letters
# 
# @usage
# randomString(length=12)
# 
# @param length  expected number of characters
# @return Random string
randomString <- function(length=12) {
  return(paste(sample(c(0:9, letters, LETTERS), length, replace=TRUE), collapse=""))
}

format_result <- function(r) {
  result <- NULL
  if(typeof(r) == "environment") result <- sprintf('<%s> (%s)', typeof(r$toString()[[1]]), r$toString())
  else                           result <- sprintf('<%s> (%s)', typeof(r), toString(r))
  return(result)
}

format_stack_entry <- function(r) {
  if(typeof(r) != 'environment') return(toString(r))
  else                           return(sprintf('<%s @ %s>', typeof(r), id(r)))
}

#-----------------------------------------------------------------------------
#                        ===  LR Parsing Engine ===
#
# The following classes are used for the LR parser itself.  These are not
# used during table construction and are independent of the actual LR
# table generation algorithm
#-----------------------------------------------------------------------------


# Non-terminal grammar symbol
# 
# This class is used to hold non-terminal grammar symbols during parsing.
# It normally has the following attributes set:
# \itemize{
#  \item type      - Grammar symbol type
#  \item value     - Symbol value
#  \item lineno    - Starting line number
#  \item endlineno - Ending line number (optional, set automatically)
#  \item lexpos    - Starting lex position
#  \item endlexpos - Ending lex position (optional, set automatically)
# }
#
# @docType class
# @importFrom R6 R6Class
# @format An \code{\link{R6Class}} generator object
YaccSymbol <- R6Class("YaccSymbol",
  public = list(
    type      = NA,
    value     = NA,
    lineno    = NA,
    endlineno = NA,
    lexpos    = NA,
    endlexpos = NA,
    toString = function() {
      return(self$type)
    }
  )
)


#' Object sent to grammar rule
#' 
#' This class is a wrapper around the objects actually passed to each
#' grammar rule. Index lookup and assignment actually assign the
#' .value attribute of the underlying YaccSymbol object.
#' The lineno() method returns the line number of a given
#' item (or 0 if not defined).   The linespan() method returns
#' a tuple of (startline,endline) representing the range of lines
#' for a symbol.  The lexspan() method returns a tuple (lexpos,endlexpos)
#' representing the range of positional information for a symbol.
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
YaccProduction <- R6Class("YaccProduction",
  public = list(
    slice = NA,
    stack = NA,
    lexer = NA,
    parser = NA,
    initialize = function(s, stack=NA) {
      self$slice <- s
      self$stack <- stack
      self$lexer <- NA
      self$parser <- NA
    },
    toString = function() {
      return(self$slice[[1]]$value)
    },
    get = function(n) {
      if(n > 0) return(self$slice[[n]]$value)
      else      return(tail(self$stack, -n)[[1]]$value)
    },
    set = function(n, value) {
      if(n > 0) self$slice[[n]]$value <- value
      else      tail(self$stack, -n)[[1]]$value <- value
    },
    length = function() {
      return(length(self$slice))
    },
    linespan = function(n) {
      startline <- self$slice[[n]]$lineno
      if(is.na(startline)) startline <- 0
      endline   <- self$slice[[n]]$endlineno
      if(is.na(endline)) endline <- startline
      return(c(startline, endline))
    },
    lexspan = function(n) {
      startpos <- self$slice[[n]]$lexpos
      if(is.na(startpos)) startpos <- 0
      endpos   <- self$slice[[n]]$endlexpos
      if(is.na(endpos)) endpos <- startpos
      return(c(startpos, endpos))
    }
  )
)


#' The LR Parsing engine
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
LRParser <- R6Class("LRParser",
  public = list(
    productions = NA,
    action      = NA,
    goto        = NA,
    errorfunc   = NA,
    errorok     = NA,
    
    defaulted_states = NA,
    statestack = NA,
    symstack = NA,
    
    state = NA,
    initialize = function(lrtab, errorf) {
      self$productions <- lrtab$lr_productions
      self$action      <- lrtab$lr_action
      self$goto        <- lrtab$lr_goto
      self$errorfunc   <- errorf
      self$set_defaulted_states()
      self$errorok     <- TRUE
    },
    # Defaulted state support.
    # This method identifies parser states where there is only one possible reduction action.
    # For such states, the parser can make a choose to make a rule reduction without consuming
    # the next look-ahead token.  This delayed invocation of the tokenizer can be useful in
    # certain kinds of advanced parsing situations where the lexer and parser interact with
    # each other or change states (i.e., manipulation of scope, lexer states, etc.).
    #
    # See:  http://www.gnu.org/software/bison/manual/html_node/Default-Reductions.html#Default-Reductions
    set_defaulted_states = function() {
      self$defaulted_states <- new.env(hash=TRUE)
      for(state in names(self$action)) {
        actions <- self$action[[state]]
        if(length(names(actions)) == 1 && actions[[names(actions)[[1]]]] < 0) {
          self$defaulted_states[[state]] <- actions[[names(actions)[[1]]]]
        }
      }
    },
    disable_defaulted_states = function() {
      self$defaulted_states <- new.env(hash=TRUE)
    },
    parse = function(input, lexer, debug=FALSE, tracking=FALSE) {
      debuglog <- NULL
      if(debug) debuglog <- RlyLogger$new()
      else      debuglog <- NullLogger$new()
      
      lookahead      <- NULL                   # Current lookahead symbol
      lookaheadstack <- list()                 # Stack of lookahead symbols
      pslice         <- YaccProduction$new(NA) # Production object passed to grammar rules
      errorcount     <- 0                      # Used during error recovery
      
      debuglog$info('RLY: PARSE DEBUG START')
      
      # Set up the lexer and parser objects on pslice
      pslice$lexer  <- lexer
      pslice$parser <- self
      
      # If input was supplied, pass to lexer
      if(!is.na(input)) lexer$input(input)
      
      # Set up the state and symbol stacks
      self$statestack <- list() # Stack of parsing states
      self$symstack   <- list() # Stack of grammar symbols
      
      pslice$stack <- self$symstack  # Put in the production
      
      # The start state is assumed to be (1,$end)
  
      self$statestack <- append(self$statestack, 1)
      sym <- YaccSymbol$new()
      sym$type <- '$end'
      self$symstack <- append(self$symstack, sym)
      state <- 1
      while(TRUE) {
        # Get the next symbol on the input.  If a lookahead symbol
        # is already set, we just use that. Otherwise, we'll pull
        # the next token off of the lookaheadstack or from the lexer
        
        debuglog$info('')
        debuglog$info(sprintf('State  : %s', state))
        
        t <- NULL
        if(state %nin% names(self$defaulted_states)) {
          if(is.null(lookahead)) {
            if(length(lookaheadstack) == 0) lookahead <- lexer$token() # Get the next token
            else {
              lookahead <- tail(lookaheadstack, 1)[[1]]
              lookaheadstack <- head(lookaheadstack, -1)
            }
            if(is.null(lookahead)) {
              lookahead <- YaccSymbol$new()
              lookahead$type <- '$end'
            }
          }
          
          # Check the action table
          ltype <- lookahead$type[[1]]
          t <- self$action[[as.character(state)]][[ltype]]
        } else {
          t <- self$defaulted_states[[as.character(state)]]
          debuglog$info(sprintf('Defaulted state %s: Reduce using %d', state, -t))
        }
        
        if(debug) {
          stack <- sprintf('%s . %s', paste(tail(sapply(self$symstack, function(x) x$type), -1), collapse=' '),
                                 if(is.null(lookahead)) "NULL" else lookahead$toString())
          debuglog$info(sprintf('Stack  : %s', stack))
        }
        
        if(!is.null(t)) {
          if(t > 0) {
            # shift a symbol on the stack
            self$statestack <- append(self$statestack, t)
            state <- t
            
            debuglog$info(sprintf('Action : Shift and goto state %s', t))
            
            self$symstack <- append(self$symstack, lookahead)
            lookahead <- NULL
            
            # Decrease error count on successful shift
            if(errorcount != 0) errorcount <- errorcount - 1
            next

          } else if(t < 0) {
            # reduce a symbol on the stack, emit a production
            p <- self$productions[[-t + 1]]
            pname <- p$name
            plen  <- p$len
            
            # Get production function
            sym <- YaccSymbol$new()
            sym$type <- pname       # Production name
            sym$value <- NULL
            
            cat('\n')
            if(plen > 0) {
              debuglog$info(sprintf('Action : Reduce rule [%s] with [%s] and goto state %d', 
                                     p$toString(), 
                                     toString(lapply(lapply(tail(self$symstack, plen), function(x) x$value), format_stack_entry)),
                                     self$goto[[as.character(tail(self$statestack, plen+1)[[1]])]][[pname]]))
            } else {
              debuglog$info(sprintf('Action : Reduce rule [%s] with [%s] and goto state %d', 
                                     p$toString(), 
                                     ' ',
                                     self$goto[[as.character(tail(self$statestack, 1)+1)]][[pname]]))
            }
            
            if(plen > 0) {
              targ <- tail(self$symstack, plen+1)
              targ[[1]] <- sym
              
              if(tracking) {
                t1 <- targ[[2]]
                sym$lineno <- t1$lineno
                sym$lexpos <- t1$lexpos
                t1 <- tail(targ, 1)[[1]]
                sym$endlineno <- t1$endlineno
                if(is.null(sym$endlineno)) sym$endlineno <- t1$lineno
                sym$endlexpos <- t1$endlexpos
                if(is.null(sym$endlexpos)) sym$endlexpos <- t1$lexpos
              }
              
              pslice$slice <- targ
              
              tryCatch({
                # Call the grammar rule with our special slice object
                self$symstack <- head(self$symstack, -plen)
                self$state <- state
                p$callable(p=pslice)
                self$statestack <- head(self$statestack, -plen)
                
                debuglog$info(sprintf('Result : %s', format_result(pslice)))
                
                self$symstack <- append(self$symstack, sym)
                state <- self$goto[[as.character(tail(self$statestack, 1)[[1]])]][[pname]]
                self$statestack <- append(self$statestack, state)
              }, error = function(e) {
                cat('YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY\n')
                # If an error was set. Enter error recovery state
                lookaheadstack <- append(lookaheadstack, lookahead)    # Save the current lookahead token
#                symstack.extend(targ[1:-1])                           # Put the production slice back on the stack
#                statestack.pop()                                      # Pop back one state (before the reduce)
#                state = statestack[-1]
#                sym.type = 'error'
#                sym.value = 'error'
#                lookahead = sym
#                errorcount = error_count
#                self.errorok = False
              })
            
              next
              
            } else {
              
              if(tracking) {
                sym$lineno <- lexer$lineno
                sym$lexpos <- lexer$lexpos
              }
              
              targ <- c(sym)
              
              # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              # The code enclosed in this section is duplicated
              # above as a performance optimization.  Make sure
              # changes get made in both locations.
  
              pslice$slice <- targ
              
              tryCatch({
                # Call the grammar rule with our special slice object
                self$state <- state
                p$callable(p=pslice)
                
                debuglog$info(sprintf('Result : %s', format_result(pslice)))
                
                self$symstack <- append(self$symstack, sym)
                state <- self$goto[[as.character(tail(self$statestack, 1)[[1]]+1)]][[pname]]
                self$statestack <- append(self$statestack, state)
              }, error = function(e) {
                cat('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n')
                # If an error was set. Enter error recovery state
                lookaheadstack <- append(lookaheadstack, lookahead)    # Save the current lookahead token
#                symstack.extend(targ[1:-1])         # Put the production slice back on the stack
#                statestack.pop()                    # Pop back one state (before the reduce)
#                state = statestack[-1]
#                sym.type = 'error'
#                sym.value = 'error'
#                lookahead = sym
#                errorcount = error_count
#                self.errorok = False
              })
  
              next
              # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            }
            
          } else if(t == 0) {
            n <- tail(self$symstack, 1)[[1]]
            result <- n[['value']]
            
            debuglog$info(sprintf('Done   : Returning %s', format_result(result)))
            debuglog$info('PLY: PARSE DEBUG END')
            
            return(result)
          }
        } else {
          
          stack <- sprintf('%s . %s', paste(tail(sapply(self$symstack, function(x) x$type), -1), collapse=' '),
                                 if(is.null(lookahead)) "NULL" else lookahead$toString())
          debuglog$error(sprintf('Error : %s', stack))
          
          # We have some kind of parsing error here.  To handle
          # this, we are going to push the current token onto
          # the tokenstack and replace it with an 'error' token.
          # If there are any synchronization rules, they may
          # catch it.
          #
          # In addition to pushing the error token, we call call
          # the user defined p_error() function if this is the
          # first syntax error.  This function is only called if
          if(errorcount == 0 || self$errorok) {
            errorcount <- error_count
            self$errorok <- FALSE
            errtoken <- lookahead
            if(errtoken$type == '$end') errtoken <- NULL
            if(!is.null(self$errorfunc)) {
              if(!is.null(errtoken))
                if(is.null(errtoken$lexer)) errtoken$lexer <- lexer
              
              self$state <- state
              tok <- self$errorfunc(errtoken)
              if(self$errorok) {
                # User must have done some kind of panic
                # mode recovery on their own.  The
                # returned token is the next lookahead
                lookahead <- tok
                errtoken <- NULL
                next
              }
            } else {
              if(errtoken) {
#                    if hasattr(errtoken, 'lineno'):
#                          lineno = lookahead.lineno
#                    else:
#                          lineno = 0
#              if lineno:
#                    sys.stderr.write('yacc: Syntax error at line %d, token=%s\n' % (lineno, errtoken.type))
#                            else:
#                                  sys.stderr.write('yacc: Syntax error, token=%s' % errtoken.type)
#                                          else:
#                                                sys.stderr.write('yacc: Parse error in input. EOF\n')
#              return
              }
            }
          } else errorcount <- error_count
          
          # case 1:  the statestack only has 1 entry on it.  If we're in this state, the
          # entire parse has been rolled back and we're completely hosed.   The token is
          # discarded and we just keep going.

          if(length(self$statestack) <= 1 && lookahead$type != '$end') {
            lookahead <- NULL
            errtoken <- NULL
            state <- 1
            # Nuke the pushback stack
            lookaheadstack <- list()
            next
          }

          # case 2: the statestack has a couple of entries on it, but we're
          # at the end of the file. nuke the top entry and generate an error token

          # Start nuking entries on the stack
          if(lookahead$type == '$end') {
            # Whoa. We're really hosed here. Bail out
            return()
          }
          
          if(lookahead$type != 'error') {
            sym <- tail(self$symstack, 1)[[1]]
            if(sym$type == 'error') {
              # Hmmm. Error is on top of stack, we'll just nuke input
              # symbol and continue
              #--! TRACKING
              if(tracking) {
                sym$endlineno <- lookahead$lineno
                if(is.na(sym$endlineno)) sym$endlineno <- sym$lineno
                sym$endlexpos <- lookahead$lexpos
                if(is.na(sym$endlexpos)) sym$endlexpos <- sym$lexpos
              }
               #--! TRACKING
               lookahead <- NULL
               next
             }
             
             # Create the error symbol for the first time and make it the new lookahead symbol
             t <- YaccSymbol$new()
             t$type <- 'error'
             
             if(!is.null(lookahead$lineno)) {
               t$lineno    <- lookahead$lineno
               t$endlineno <- lookahead$lineno
             }
             if(!is.null(lookahead$lexpos)) {
               t$lexpos    <- lookahead$lexpos
               t$endlexpos <- lookahead$lexpos
             }  
             t$value <- lookahead
             lookaheadstack <- append(lookaheadstack, lookahead)
             lookahead <- t
          } else {
            sym <- tail(self$symstack, 1)[[1]]
            self$symstack <- head(self$symstack, -1)
            #--! TRACKING
            if(tracking) {
              lookahead$lineno <- sym$lineno
              lookahead$lexpos <- sym$lexpos             
            }
            #--! TRACKING
            self$statestack <- head(self$statestack, -1)
            state <- tail(self$statestack, 1)[[1]]
          }
          
          next
        }
        
        # Call an error function here
        stop('yacc: internal parser error!!!\n')
      }
    }
  )
)


# -----------------------------------------------------------------------------
#                          === Grammar Representation ===
#
# The following functions, classes, and variables are used to represent and
# manipulate the rules that make up a grammar.
# -----------------------------------------------------------------------------

# regex matching identifiers
is_identifier <- '^[a-zA-Z0-9_-]+$'


# Production
#
# This class stores the raw information about a single production or grammar rule.
# A grammar rule refers to a specification such as this:
#
#       expr : expr PLUS term
#
# Here are the basic attributes defined on all productions
#
#       name     - Name of the production.  For example 'expr'
#       prod     - A list of symbols on the right side ['expr','PLUS','term']
#       prec     - Production precedence level
#       number   - Production number.
#       func     - Function that executes on reduce
#
# The following attributes are defined or optional.
#
#       len       - Length of the production (number of symbols on right hand side)
#       usyms     - Set of unique symbols found in the production
# -----------------------------------------------------------------------------
#
# @docType class
# @importFrom R6 R6Class
# @format An \code{\link{R6Class}} generator object
# @keywords data
Production <- R6Class("Production",
  public = list(
    reduced   = 0,
    name      = NA,
    prod      = NA,
    number    = NA,
    func      = NA,
    callable  = NA,
    prec      = NA,
    len       = NA,
    usyms     = NA,
    lr_items  = NA,
    lr_next   = NA,
    lr0_added = NA,
    initialize = function(number, name, prod, precedence=NA, func=NA) {
      self$name     <- name
      self$prod     <- prod
      self$number   <- number
      self$func     <- func
      self$callable <- NA
      self$prec     <- precedence
      
      # Internal settings used during table construction
          
      self$len <- length(self$prod)   # Length of the production
      
      # Create a list of unique production symbols used in the production
      self$usyms <- list()
      for(s in self$prod) {
        if(s %nin% self$usyms) self$usyms[[length(self$usyms)+1]] <- s 
      }
      
      # List of all LR items for the production
      self$lr_items <- list()
      self$lr_next  <- NA
    },
    # Return the nth lr_item from the production (or None if at the end)
    lr_item = function(n) {
    },
    toString = function() {
      return(sprintf('%s -> %s', self$name, paste(self$prod, collapse = ' ')))
    },
    bind = function(instance) {
      if(!is.na(self$func)) self$callable <- instance[[self$func]]
    }
  )
)


# LRItem
#
# This class represents a specific stage of parsing a production rule.  For
# example:
#
#       expr : expr . PLUS term
#
# In the above, the "." represents the current location of the parse.  Here
# basic attributes:
#
#       name       - Name of the production.  For example 'expr'
#       prod       - A list of symbols on the right side ['expr','.', 'PLUS','term']
#       number     - Production number.
#
#       lr_next      Next LR item. Example, if we are ' expr -> expr . PLUS term'
#                    then lr_next refers to 'expr -> expr PLUS . term'
#       lr_index   - LR item index (location of the ".") in the prod list.
#       lookaheads - LALR lookahead symbols for this item
#       len        - Length of the production (number of symbols on right hand side)
#       lr_after    - List of all productions that immediately follow
#       lr_before   - Grammar symbol immediately before
# -----------------------------------------------------------------------------
#
# @docType class
# @importFrom R6 R6Class
# @format An \code{\link{R6Class}} generator object
# @keywords data
LRItem <- R6Class("LRItem",
  public = list(
    name       = NA,
    prod       = NA,
    number     = NA,
    lr_index   = NA,
    lookaheads = NA,
    len        = NA,
    usyms      = NA,
    
    lr_after   = NA,
    lr_before  = NA,
    lr_next    = NA,
    initialize = function(p, n) {
      self$name       <- p$name
      self$prod       <- p$prod
      self$number     <- p$number
      self$lr_index   <- n
      self$lookaheads <- new.env(hash=TRUE)
      if(n == 1) self$prod <- c('.', self$prod)
      else       self$prod <- append(self$prod, '.', n - 1)
      self$len        <- length(self$prod)
      self$usyms      <- p$usyms
    },
    toString = function() {
      s <- ''
      if(!is.null(self$prod)) s <- sprintf('%s -> %s', self$name, paste(self$prod, collapse=' '))
      else                    s <- sprintf('%s -> <empty>',  self$name)
      return(s)
    }
  )
)

# Rightmost terminal
#
# Return the rightmost terminal from a list of symbols.  Used in add_production()
# 
# @param symbols list of symbols
# @param terminals list of terminals
# 
# @return rightmost terminal
rightmost_terminal = function(symbols, terminals) {
  i <- length(symbols) - 1
  while(i >= 1) {
    if(symbols[[i]] %in% names(terminals)) return(symbols[i])
    i <- i - 1
  }
  return(NULL)
}


# Grammar
#
# The following class represents the contents of the specified grammar along
# with various computed properties such as first sets, follow sets, LR items, etc.
# This data is used for critical parts of the table generation process later.
#
# @docType class
# @importFrom R6 R6Class
# @format An \code{\link{R6Class}} generator object
Grammar <- R6Class("Grammar",
  public = list(
    Productions    = NA,
    Prodnames      = NA,
    Prodmap        = NA,
    Terminals      = NA,
    Nonterminals   = NA,
    First          = NA,
    Follow         = NA,
    Precedence     = NA,
    UsedPrecedence = NA,
    Start          = NA,
    initialize = function(terminals) {
      self$Productions <- list(NULL)       # - A list of all of the productions.  The first
                                           #   entry is always reserved for the purpose of
                                           #   building an augmented grammar      
      self$Prodnames <- new.env(hash=TRUE) # - A dictionary mapping the names of nonterminals to a list of all
                                           #   productions of that nonterminal.
      self$Prodmap   <- new.env(hash=TRUE) # - A dictionary that is only used to detect duplicate
                                           #   productions.
      self$Terminals <- new.env(hash=TRUE) # - A dictionary mapping the names of terminal symbols to a
                                           #   list of the rules where they are used.
      
      for(term in terminals) {
        self$Terminals[[term]] <- c()
      }
      
      self$Terminals[['error']] <- c()
      
      self$Nonterminals <- new.env(hash=TRUE) # - A dictionary mapping names of nonterminals to a list
                                              #   of rule numbers where they are used.
      self$First      <- new.env(hash=TRUE)   # - A dictionary of precomputed FIRST(x) symbols
      self$Follow     <- new.env(hash=TRUE)   # - A dictionary of precomputed FOLLOW(x) symbols
      self$Precedence <- new.env(hash=TRUE)   # - Precedence rules for each terminal. Contains tuples of the
                                              #   form ('right',level) or ('nonassoc', level) or ('left',level)
      self$UsedPrecedence <- list()           # - Precedence rules that were actually used by the grammer.
                                              # - This is only used to provide error checking and to generate
                                              #   a warning about unused precedence rules.
      self$Start <- NA                        # - Starting symbol for the grammar
    },
    # -----------------------------------------------------------------------------
    # set_precedence()
    #
    # Sets the precedence for a given terminal. assoc is the associativity such as
    # 'left','right', or 'nonassoc'.  level is a numeric level.
    #
    # -----------------------------------------------------------------------------
    set_precedence = function(term, assoc, level) {
      if(length(self$Productions) > 1)                 stop('Must call set_precedence() before add_production()')
      if(term %in% names(self$recedence))              stop(sprintf('Precedence already specified for terminal %s', term))
      if(!(assoc %in% c('left', 'right', 'nonassoc'))) stop("Associativity must be one of 'left','right', or 'nonassoc'")
      self$Precedence[[term]] <- list(assoc, level) 
    },
    # -----------------------------------------------------------------------------
    # add_production()
    #
    # Given an action function, this function assembles a production rule and
    # computes its precedence level.
    #
    # The production rule is supplied as a list of symbols.   For example,
    # a rule such as 'expr : expr PLUS term' has a production name of 'expr' and
    # symbols ['expr','PLUS','term'].
    #
    # Precedence is determined by the precedence of the right-most non-terminal
    # or the precedence of a terminal specified by %prec.
    #
    # A variety of error checks are performed to make sure production symbols
    # are valid and that %prec is used correctly.
    # -----------------------------------------------------------------------------
    add_production = function(prodname, syms, func) {
      if(prodname %in% names(self$Terminals))        stop(sprintf('%s: Illegal rule name %s. Already defined as a token', func, prodname))
      if(prodname == 'error')                        stop(sprintf('%s: Illegal rule name %s. error is a reserved word', func, prodname))
      if(!grepl(is_identifier, prodname, perl=TRUE)) stop(sprintf('%s: Illegal rule name %s', func, prodname))
      
      # Look for literal tokens
      n <- 0
      for(s in syms) {
        n <- n + 1
        if(substr(s, 1, 1) %in% c("'", "\"")) {
          c <- eval(parse(text=s))
          if(nchar(c) > 1) stop(sprintf('%s: Literal token %s in rule %s may only be a single character', func, s, prodname))
          if(!(c %in% names(self$Terminals))) self$Terminals[[c]] <- c()
          syms[n] <- c
          next
        }
        if(!grepl(is_identifier, s, perl=TRUE) && s != '%prec') stop(sprintf('%s: Illegal name %s in rule %s', func,s, prodname))
      }
            
      # Determine the precedence level
      if('%prec' %in% syms) {
        if(syms[length(syms)]     == '%prec') stop(sprintf('%s: Syntax error. Nothing follows %%prec', func))
        if(syms[length(syms) - 1] != '%prec') stop(sprintf('%s: Syntax error. %%prec can only appear at the end of a grammar rule', func))
       
        precname <- syms[length(syms)]
        prodprec <- self$Precedence[[precname]]
       
        if(is.null(prodprec)) stop(sprintf('%s: Nothing known about the precedence of %s', func, precname))
        else self$UsedPrecedence[[length(self$UsedPrecedence)+1]] <- precname
        # Drop %prec from the rule
        syms <- head(syms,-2)
      } else {
        # If no %prec, precedence is determined by the rightmost terminal symbol
        precname <- rightmost_terminal(syms, self$Terminals)
        if(!is.null(precname)) prodprec <- self$Precedence[[precname]]
        else prodprec <- NULL
        
        if(is.null(prodprec)) prodprec <- list('right', 0)
      }
    
      # See if the rule is already in the rulemap
      map <- sprintf('%s -> %s', prodname, toString(syms))
      if(map %in% names(self$Prodmap)) {
        m <- self$Prodmap[[map]]
        stop(sprintf('%s: Duplicate rule %s. ', func, m))
      }
        
      # From this point on, everything is valid.  Create a new Production instance
      pnumber <- length(self$Productions)
      if(!(prodname %in% names(self$Nonterminals))) self$Nonterminals[[prodname]] <- c()

      # Add the production number to Terminals and Nonterminals
      for(t in syms) {
        if(t %in% names(self$Terminals)) self$Terminals[[t]][[length(self$Terminals[[t]])+1]] <- pnumber
        else {
          if(!(t %in% names(self$Nonterminals))) self$Nonterminals[[t]] <- c()
          self$Nonterminals[[t]][[length(self$Nonterminals[[t]])+1]] <- pnumber
        }
      }
      
      # Create a production and add it to the list of productions
      p <- Production$new(pnumber, prodname, syms, prodprec, func)
      self$Productions[[length(self$Productions)+1]] <- p
      self$Prodmap[[map]] <- p
    
      # Add to the global productions list
      if(!(prodname %in% names(self$Prodnames))) self$Prodnames[[prodname]] <- c()
      self$Prodnames[[prodname]][[length(self$Prodnames[[prodname]])+1]] <- p
    },
    # -----------------------------------------------------------------------------
    # set_start()
    #
    # Sets the starting symbol and creates the augmented grammar.  Production
    # rule 0 is S' -> start where start is the start symbol.
    # -----------------------------------------------------------------------------
    set_start = function(start=NA) {
      if(is.na(start)) start <- self$Productions[[2]]$name
      if(!(start %in% names(self$Nonterminals))) stop(sprintf('start symbol %s undefined', start))
      self$Productions[[1]] <- Production$new(0, "S'", c(start))
      self$Nonterminals[[start]][[length(self$Nonterminals[[start]])+1]] <- 0
      self$Start <- start
    },
    # -----------------------------------------------------------------------------
    # find_unreachable()
    #
    # Find all of the nonterminal symbols that can't be reached from the starting
    # symbol.  Returns a list of nonterminals that can't be reached.
    # -----------------------------------------------------------------------------
    find_unreachable = function() {
      reachable <- c()
      reachable <- private$mark_reachable_from(reachable, self$Productions[[1]]$prod[[1]])
      return(setdiff(names(self$Nonterminals), reachable))
    },
    # -----------------------------------------------------------------------------
    # infinite_cycles()
    #
    # This function looks at the various parsing rules and tries to detect
    # infinite recursion cycles (grammar rules where there is no possible way
    # to derive a string of only terminals).
    # -----------------------------------------------------------------------------
    infinite_cycles = function() {
      terminates <- new.env(hash=TRUE)
      
      # Terminals:
      for(t in names(self$Terminals)) terminates[[t]] <- TRUE
      
      terminates[['$end']] <- TRUE
      
      # Nonterminals:
  
      # Initialize to false:
      for(n in names(self$Nonterminals)) terminates[[n]] <- FALSE
      
      # Then propagate termination until no change:
      while(TRUE) {
        some_change <- FALSE
        for(n in names(self$Prodnames)) {
          pl <- self$Prodnames[[n]]
          # Nonterminal n terminates if any of its productions terminates.
          for(p in pl) {
            # Production p terminates if all of its rhs symbols terminate.
        
            # didn't break from the loop,
            # so every symbol s terminates
            # so production p terminates.
            p_terminates <- TRUE
            for(s in p$prod) {
              if(!terminates[[s]]) {
                # The symbol s does not terminate,
                # so production p does not terminate.
                p_terminates <- FALSE
                break
              }
            }
            
            if(p_terminates) {
              # symbol n terminates!
              if(!terminates[[n]]) {
                terminates[[n]] <- TRUE
                some_change <- TRUE
              }
              # Don't need to consider any more productions for this n.
              break
            }
          }
        }
        if(!some_change) break
      }
      
      infinite <- list()
      for(s in names(terminates)) {
        term <- terminates[[s]]
        if(!term) {
          if(s %nin% names(self$Prodnames) && s %nin% names(self$Terminals) && s != 'error') { }
          else infinite[[length(infinite)+1]] <- s
        }
      }
      
      return(infinite)
    },
    # -----------------------------------------------------------------------------
    # undefined_symbols()
    #
    # Find all symbols that were used the grammar, but not defined as tokens or
    # grammar rules.  Returns a list of tuples (sym, prod) where sym in the symbol
    # and prod is the production where the symbol was used.
    # -----------------------------------------------------------------------------
    undefined_symbols = function() {
      result <- list()
      for(p in self$Productions) {
        if(is.null(p)) next
        for(s in p$prod) {
          if(!(s %in% names(self$Prodnames)) && !(s %in% names(self$Terminals)) && s != 'error')
            result[[length(result)+1]] <- c(s, p)
        }
      }
      return(result)
    },
    # -----------------------------------------------------------------------------
    # unused_terminals()
    #
    # Find all terminals that were defined, but not used by the grammar.  Returns
    # a list of all symbols.
    # -----------------------------------------------------------------------------
    unused_terminals = function() {
      unused_tok <- list()
      for(s in names(self$Terminals)) {
        v <- self$Terminals[[s]]
        if(s != 'error' && length(v) == 0)
          unused_tok[[length(unused_tok)+1]] <- s
      }
      return(unused_tok)
    },
    # ------------------------------------------------------------------------------
    # unused_rules()
    #
    # Find all grammar rules that were defined,  but not used (maybe not reachable)
    # Returns a list of productions.
    # ------------------------------------------------------------------------------
    unused_rules = function() {
      unused_rules <- list()
      for(s in names(self$Nonterminals)) {
		    v <- self$Nonterminals[[s]]
		    if(is.null(v)) {
		      p <- self$Prodnames[[s]][[1]]
		      unused_rules[[length(unused_rules)+1]] <- p
		    }
	    }
      return(unused_rules)
    },
    # -----------------------------------------------------------------------------
    # unused_precedence()
    #
    # Returns a list of tuples (term,precedence) corresponding to precedence
    # rules that were never used by the grammar.  term is the name of the terminal
    # on which precedence was applied and precedence is a string such as 'left' or
    # 'right' corresponding to the type of precedence.
    # -----------------------------------------------------------------------------
    unused_precedence = function() {
      unused <- list()
      for(termname in names(self$Precedence)) {
        if(!(termname %in% names(self$Terminals) || termname %in% self$UsedPrecedence))
          unused[[length(unused)+1]] <- c(termname, self$Precedence[[termname]][[1]])
      }
      return(unused)
    },
    # -------------------------------------------------------------------------
    # _first()
    #
    # Compute the value of FIRST1(beta) where beta is a tuple of symbols.
    #
    # During execution of compute_first1, the result may be incomplete.
    # Afterward (e.g., when called from compute_follow()), it will be complete.
    # -------------------------------------------------------------------------
    first = function(beta) {

      # We are computing First(x1,x2,x3,...,xn)
      result <- c()
      broke <- FALSE
      for(x in beta) {
        x_produces_empty <- FALSE
        
        # Add all the non-<empty> symbols of First[x] to the result.
        for(f in self$First[[x]]) {
          if(f == '<empty>') x_produces_empty <- TRUE
          else if(f %nin% result) result <- append(result, f)
        }
        
        if(x_produces_empty) {
          # We have to consider the next x in beta,
          # i.e. stay in the loop.         
        } else {
          # We don't have to consider any further symbols in beta.
          broke <- TRUE
          break
        }
      }
      
      if(!broke) result <- append(result, '<empty>')
      
      return(result)
    },
    # -------------------------------------------------------------------------
    # compute_first()
    #
    # Compute the value of FIRST1(X) for all symbols
    # -------------------------------------------------------------------------
    compute_first = function() {
      if(length(names(self$First)) > 0) return(self$First)

      # Terminals:
      for(t in names(self$Terminals)) self$First[[t]] <- c(t)

      self$First[['$end']] <- c('$end')
      
      # Nonterminals:
  
      # Initialize to the empty set:
      for(n in names(self$Nonterminals)) self$First[[n]] <- c()
      
      # Then propagate symbols until no change:
      while(TRUE) {
        some_change <- FALSE
        for(n in names(self$Nonterminals)) {
          for(p in self$Prodnames[[n]]) {
            for(f in self$first(p$prod)) {
              if(f %nin% self$First[[n]]) {
                self$First[[n]] <- append(self$First[[n]], f)
                some_change <- TRUE
              }
            }
          }
        }
        if(!some_change) break
      }
  
      return(self$First)
    },
    # ---------------------------------------------------------------------
    # compute_follow()
    #
    # Computes all of the follow sets for every non-terminal symbol.  The
    # follow set is the set of all symbols that might follow a given
    # non-terminal.  See the Dragon book, 2nd Ed. p. 189.
    # ---------------------------------------------------------------------
    compute_follow = function(start=NA) {
      # If already computed, return the result
      if(length(names(self$Follow)) > 0) return(self$Follow)
      
      # If first sets not computed yet, do that first.
      if(length(names(self$First)) == 0) self$compute_first()
      
      # Add '$end' to the follow list of the start symbol
      for(k in names(self$Nonterminals)) self$First[[k]] <- c()
      
      if(is.na(start)) start <- self$Productions[[1]]$name
      
      self$Follow[[start]] <- c('$end')
      
      while(TRUE) {
        didadd <- FALSE
        for(p in tail(self$Productions, -1)) {
          # Here is the production set
          i <- 1
          for(B in p$prod) {
            if(B %in% names(self$Nonterminals)) {
              # Okay. We got a non-terminal in a production
              fst <- self$first(tail(p$prod, -i))
              hasempty <- FALSE
              for(f in fst) {
                if(f != '<empty>' && f %nin% self$Follow[[B]]) {
                  self$Follow[[B]] <- append(self$Follow[[B]], f)
                  didadd <- TRUE
                }
                if(f == '<empty>') hasempty <- TRUE
              }
              if(hasempty || i == (length(p$prod)-1)) {
                # Add elements of follow(a) to follow(b)
                for(f in self$Follow[[p$name]]) {
                  if(f %nin% self$Follow[[B]]) {
                    self$Follow[[B]] <- append(self$Follow[[B]], f)
                    didadd <- TRUE
                  }
                }
              }
            }
            i <- i + 1
          }
        }
        if(!didadd) break
      }
    },
    # -----------------------------------------------------------------------------
    # build_lritems()
    #
    # This function walks the list of productions and builds a complete set of the
    # LR items.  The LR items are stored in two ways:  First, they are uniquely
    # numbered and placed in the list _lritems.  Second, a linked list of LR items
    # is built for each production.  For example:
    #
    #   E -> E PLUS E
    #
    # Creates the list
    #
    #  [E -> . E PLUS E, E -> E . PLUS E, E -> E PLUS . E, E -> E PLUS E . ]
    # -----------------------------------------------------------------------------
    build_lritems = function() {
      for(p in self$Productions) {
        lastlri <- p
        i <- 0
        lr_items <- list()
        lri <- NA
        while(TRUE) {
          if(i > p$len) lri <- NULL
          else {
            lri <- LRItem$new(p, (i+1))
            # Precompute the list of productions immediately following
            tryCatch({
              lri$lr_after <- self$Prodnames[[lri$prod[[i+2]]]]
            }, error = function(e) {
              lri$lr_after <- list()
            })
            tryCatch({
              lri$lr_before <- lri$prod[[i]]
            }, error = function(e) {
              lri$lr_before <- NULL
            })
          }
          
          lastlri$lr_next <- lri
          if(is.null(lri)) break
          lr_items[[length(lr_items)+1]] <- lri
          lastlri <- lri
          i <- i + 1
        }
        p$lr_items <- lr_items
      }
    }
  ),
  private = list(
    # Mark all symbols that are reachable from a symbol s
    mark_reachable_from = function(reachable, s) {
      if(s %in% reachable) return(reachable)
      reachable[[length(reachable)+1]] <- s
      reachable <- unique(reachable)
      for(p in self$Prodnames[[s]])
          for(r in p$prod)
            reachable <- private$mark_reachable_from(reachable, r)
        
      return(reachable)
    }
  )
)


# -----------------------------------------------------------------------------
#                           === LR Generator ===
#
# The following classes and functions are used to generate LR parsing tables on
# a grammar.
# -----------------------------------------------------------------------------


#  LRGeneratedTable
#
# This class implements the LR table generation algorithm.  There are no
# public methods
# -----------------------------------------------------------------------------
#
# @docType class
# @importFrom R6 R6Class
# @format An \code{\link{R6Class}} generator object
# @keywords data
LRGeneratedTable <- R6Class("LRGeneratedTable",
  public = list(
    grammar        = NA,
    lr_method      = NA,
    log            = NA,
    lr_action      = NA,
    lr_goto        = NA,
    lr_productions = NA,
    lr_goto_cache  = NA,
    lr0_cidhash    = NA,
    sr_conflict    = NA,
    rr_conflict    = NA,
    conflicts      = NA,
    sr_conflicts   = NA,
    rr_conflicts   = NA,
    initialize = function(grammar, method='LALR', log=NULL) {
      if(method %nin% c('SLR', 'LALR')) stop(sprintf('Unsupported method %s', method))
      
      self$grammar   <- grammar
      self$lr_method <- method
      
      # Set up the logger
      if(is.null(log)) log <- NullLogger$new()
      self$log <- log
      
      # Internal attributes
      self$lr_action      <- new.env(hash=TRUE)  # Action table
      self$lr_goto        <- new.env(hash=TRUE)  # Goto table
      self$lr_productions <- grammar$Productions # Copy of grammar Production array
      self$lr_goto_cache  <- new.env(hash=TRUE)  # Cache of computed gotos
      self$lr0_cidhash    <- new.env(hash=TRUE)  # Cache of closures      

      private$add_count   <- 0                   # Internal counter used to detect cycles
      
      # Diagonistic information filled in by the table generator
      self$sr_conflict <- 0
      self$rr_conflict <- 0
      self$conflicts   <- list()        # List of conflicts
          
      self$sr_conflicts <- list()
      self$rr_conflicts <- list()
          
      # Build the tables
      self$grammar$build_lritems()
      self$grammar$compute_first()
      self$grammar$compute_follow()
      self$lr_parse_table()
    },
    # Compute the LR(0) closure operation on I, where I is a set of LR(0) items.
    lr0_closure = function(I) {
      private$add_count <- private$add_count + 1
      
      # Add everything in I to J
#      J <- append(I, rep(NULL, 10))
      J <- I
      didadd <- TRUE
      while(didadd) {
        didadd <- FALSE
        i <- 1
        while(i <= length(J[!is.null(J)])) {
          j <- J[[i]]
          for(x in j$lr_after) {
            if     ( is.na(x$lr0_added) && private$add_count == 0)           next
            else if(!is.na(x$lr0_added) && x$lr0_added == private$add_count) next
            # Add B --> .G to J
            J[[length(J[!is.null(J)])+1]] <- x$lr_next
            x$lr0_added <- private$add_count
            didadd <- TRUE
          }
          i <- i + 1
        }
      }
      
#      J <- J[!is.null(J)]
      return(J)
    },
    # Compute the LR(0) goto function goto(I,X) where I is a set
    # of LR(0) items and X is a grammar symbol.   This function is written
    # in a way that guarantees uniqueness of the generated goto sets
    # (i.e. the same goto set will never be returned as two different Python
    # objects).  With uniqueness, we can later do fast set comparisons using
    # id(obj) instead of element-wise comparison.
    lr0_goto = function(I, x) {
      # First we look for a previously cached entry
      g <- self$lr_goto_cache[[paste(id(I), x)]]
      if(!is.null(g)) return(g)
      
      # Now we generate the goto set in a way that guarantees uniqueness
      # of the result
  
      s <- self$lr_goto_cache[[x]]
      if(is.null(s)) {
        s <- new.env(hash=TRUE)
        self$lr_goto_cache[[x]] <- s
      }

      gs <- c()
      for(p in I) {
        n <- p$lr_next
        if(!is.null(n) && n$lr_before == x) {
          s1 <- s[[id(n)]]
          if(is.null(s1)) {
            s1 <- new.env(hash=TRUE)
            s[[id(n)]] <- s1
          }
          gs <- append(gs, n)
          s <- s1
        }
      }
      g <- s[['$end']]
      if(is.null(g)) {
        if(length(gs) > 0) {
          g <- self$lr0_closure(gs)
          s[['$end']] <- g
        } else s[['$end']] <- gs
      }
      
      self$lr_goto_cache[[paste(id(I), x)]] <- g
      return(g)
    },
    # Compute the LR(0) sets of item function
    lr0_items = function() {
      C <- list(self$lr0_closure(c(self$grammar$Productions[[1]]$lr_next)))

      i <- 1
      for(I in C) {
        self$lr0_cidhash[[id(I)]] <- i
        i <- i + 1
      }
      
      # Loop over the items in C and each grammar symbols
      i <- 1
      while(i <= length(C)) {
        I <- C[[i]]
        i <- i + 1
        
        # Collect all of the symbols that could possibly be in the goto(I,X) sets
        asyms <- new.env(hash=TRUE)
        for(ii in I) {
          for(s in ii$usyms) {
            asyms[[s]] <- NULL
          }
        }
        
        for(x in names(asyms)) {
          g <- self$lr0_goto(I, x)
          if(is.null(g) || id(g) %in% names(self$lr0_cidhash)) next
          self$lr0_cidhash[[id(g)]] <- length(C) + 1
          C[[length(C)+1]] <- g
        }
      }
            
      return(C)
    },
    
    # -----------------------------------------------------------------------------
    #                       ==== LALR(1) Parsing ====
    #
    # LALR(1) parsing is almost exactly the same as SLR except that instead of
    # relying upon Follow() sets when performing reductions, a more selective
    # lookahead set that incorporates the state of the LR(0) machine is utilized.
    # Thus, we mainly just have to focus on calculating the lookahead sets.
    #
    # The method used here is due to DeRemer and Pennelo (1982).
    #
    # DeRemer, F. L., and T. J. Pennelo: "Efficient Computation of LALR(1)
    #     Lookahead Sets", ACM Transactions on Programming Languages and Systems,
    #     Vol. 4, No. 4, Oct. 1982, pp. 615-649
    #
    # Further details can also be found in:
    #
    #  J. Tremblay and P. Sorenson, "The Theory and Practice of Compiler Writing",
    #      McGraw-Hill Book Company, (1985).
    #
    # -----------------------------------------------------------------------------
    
    # -----------------------------------------------------------------------------
    # compute_nullable_nonterminals()
    #
    # Creates a dictionary containing all of the non-terminals that might produce
    # an empty production.
    # -----------------------------------------------------------------------------
    compute_nullable_nonterminals = function() {
      nullable <- c()
      num_nullable <- 0
      while(TRUE) {
        for(p in tail(self$grammar$Productions, -1)) {
          if(p$len == 0) {
            if(p$name %nin% nullable) nullable <- append(nullable, p$name)
            next
          }
          broke <- FALSE
          for(t in p$prod) {
            if(t %nin% nullable) {
              broke <- TRUE
              break
            }
          }
          if(!broke) {
            if(p$name %nin% nullable) nullable <- append(nullable, p$name)
          }
        }
        if(length(nullable) == num_nullable) break
        num_nullable <- length(nullable)
      }
      return(nullable)      
    },
    # -----------------------------------------------------------------------------
    # find_nonterminal_trans(C)
    #
    # Given a set of LR(0) items, this functions finds all of the non-terminal
    # transitions.    These are transitions in which a dot appears immediately before
    # a non-terminal.   Returns a list of tuples of the form (state,N) where state
    # is the state number and N is the nonterminal symbol.
    #
    # The input C is the set of LR(0) items.
    # -----------------------------------------------------------------------------
    find_nonterminal_transitions = function(C) {
      trans <- list()
      stateno <- 1
      for(state in C) {
        for(p in state) {
          if(p$lr_index < p$len) {
            t <- c(stateno, p$prod[[p$lr_index+1]])
            if(t[[2]] %in% names(self$grammar$Nonterminals)) {
              exist <- FALSE
              for(i in trans) {
                if(i[[1]] == t[[1]] && i[[2]] == t[[2]]) exist <- TRUE
              }
              if(!exist) trans[[length(trans)+1]] <- t
            }
          }
        }
        stateno <- stateno + 1
      }
      return(trans)
    },
    # -----------------------------------------------------------------------------
    # dr_relation()
    #
    # Computes the DR(p,A) relationships for non-terminal transitions.  The input
    # is a tuple (state,N) where state is a number and N is a nonterminal symbol.
    #
    # Returns a list of terminals.
    # -----------------------------------------------------------------------------
    dr_relation = function(C, trans, nullable) {
      dr_set <- new.env(hash=TRUE)
      state <- as.numeric(trans[1])
      N <- trans[2]
      terms <- c()
      
      g <- self$lr0_goto(C[[state]], N)
      for(p in g) {
        if(p$lr_index < p$len) {
          a <- p$prod[[p$lr_index+1]]
          if(a %in% names(self$grammar$Terminals))
            if(a %nin% terms) terms <- append(terms, a)
        }
      }
      
      # This extra bit is to handle the start state
      if(state == 1 && N == self$grammar$Productions[[1]]$prod[[1]])
        terms <- append(terms, '$end')
      
      return(terms)
    },
    # -----------------------------------------------------------------------------
    # reads_relation()
    #
    # Computes the READS() relation (p,A) READS (t,C).
    # -----------------------------------------------------------------------------
    reads_relation = function(C, trans, empty) {
      # Look for empty transitions
      rel <- list()
      state <- as.numeric(trans[1])
      N <- trans[2]
      
      g <- self$lr0_goto(C[[state]], N)
      j <- self$lr0_cidhash[[id(g)]]
      for(p in g) {
        if(p$lr_index < p$len - 1) {
          a <- p$prod[[p$lr_index + 1]]
          if(a %in% empty) rel[[length(rel)+1]] <- c(j, a)
        }
      }
      return(rel)
    },
    # -----------------------------------------------------------------------------
    # compute_lookback_includes()
    #
    # Determines the lookback and includes relations
    #
    # LOOKBACK:
    #
    # This relation is determined by running the LR(0) state machine forward.
    # For example, starting with a production "N : . A B C", we run it forward
    # to obtain "N : A B C ."   We then build a relationship between this final
    # state and the starting state.   These relationships are stored in a dictionary
    # lookdict.
    #
    # INCLUDES:
    #
    # Computes the INCLUDE() relation (p,A) INCLUDES (p',B).
    #
    # This relation is used to determine non-terminal transitions that occur
    # inside of other non-terminal transition states.   (p,A) INCLUDES (p', B)
    # if the following holds:
    #
    #       B -> LAT, where T -> epsilon and p' -L-> p
    #
    # L is essentially a prefix (which may be empty), T is a suffix that must be
    # able to derive an empty string.  State p' must lead to state p with the string L.
    #
    # -----------------------------------------------------------------------------
    compute_lookback_includes = function(C, trans, nullable) {
      lookdict <- new.env(hash=TRUE)       # Dictionary of lookback relations
      includedict <- new.env(hash=TRUE)    # Dictionary of include relations
      
      # Make a dictionary of non-terminal transitions
      dtrans <- new.env(hash=TRUE)
      for(t in trans) dtrans[[paste(t, collapse = ' ')]] <- 1
      
      # Loop over all transitions and compute lookbacks and includes
      for(state_N in trans) {
        state <- as.numeric(state_N[1])
        N <- state_N[2]
        lookb <- list()
        includes <- list()
        for(p in C[[state]]) {
          if(p$name != N) next
          
          # Okay, we have a name match.  We now follow the production all the way
          # through the state machine until we get the . on the right hand side
      
          lr_index <- p$lr_index
          j <- state
          
          while(lr_index < p$len) {
            lr_index <- lr_index + 1
            t <- p$prod[[lr_index]]
            
            # Check to see if this symbol and state are a non-terminal transition
            if(paste(c(j, t), collapse=' ') %in% names(dtrans)) {
              # Yes.  Okay, there is some chance that this is an includes relation
              # the only way to know for certain is whether the rest of the
              # production derives empty
              
              li <- lr_index + 1
              broke <- FALSE
              while(li < p$len + 1) {
                if(p$prod[[li]] %in% names(self$grammar$Terminals)) {
                  broke <- TRUE
                  break      # No forget it
                }
                if(p$prod[[li]] %nin% nullable) {
                  broke <- TRUE
                  break
                }
                li <- li + 1
              }
              if(!broke) {
                # Appears to be a relation between (j,t) and (state,N)
                includes[[length(includes)+1]] <- c(j, t)
              }
            }
            
            g <- self$lr0_goto(C[[j]], t)        # Go to next set
            j <- self$lr0_cidhash[[id(g)]]       # Go to next state
          }
          
          # When we get here, j is the final state, now we have to locate the production
          for(r in C[[j]]) {
            if(r$name != p$name) next
            if(r$len != p$len) next
            i <- 1
            # This look is comparing a production ". A B C" with "A B C ."
            broke <- FALSE
            while(i < r$lr_index) {
              if(r$prod[[i]] != p$prod[[i+1]]) {
                broke <- TRUE
                break
              }
              i <- i + 1
            }
            if(!broke) lookb[[length(lookb)+1]] <- c(j, r)
          }
        }
        for(i in includes) {
          if(paste(i, collapse=' ') %nin% names(includedict)) {
            includedict[[paste(i, collapse=' ')]] <- list()
          }
          includedict[[paste(i, collapse=' ')]][[length(includedict[[paste(i, collapse=' ')]])+1]] <- c(state, N)
        }
        lookdict[[paste(c(state, N), collapse=' ')]] <- lookb
      }
      return(list(lookdict, includedict))
    },
    # -----------------------------------------------------------------------------
    # digraph()
    # traverse()
    #
    # The following two functions are used to compute set valued functions
    # of the form:
    #
    #     F(x) = F'(x) U U{F(y) | x R y}
    #
    # This is used to compute the values of Read() sets as well as FOLLOW sets
    # in LALR(1) generation.
    #
    # Inputs:  X    - An input set
    #          R    - A relation
    #          FP   - Set-valued function
    # ------------------------------------------------------------------------------
    digraph = function(X, C, nullable, readsets, inclsets) {
      N <- new.env(hash=TRUE)
      for(x in X) N[[paste(x, collapse = ' ')]] <- 0
      stack <- c()
      F <- new.env(hash=TRUE)
      for(x in X) {
        if(N[[paste(x, collapse = ' ')]] == 0) {
          stack <- self$traverse(x, N, stack, F, C, nullable, readsets, inclsets)
        }
      }

      return(F)
    },
    traverse = function(x, N, stack, F, C, nullable, readsets, inclsets) {
      x_id <- paste(x, collapse = ' ')
      stack[[length(stack)+1]] <- x
      d <- length(stack)
      N[[paste(x, collapse = ' ')]] <- d
      if(!is.null(C)) F[[x_id]] <- self$dr_relation(C, x, nullable) # F(X) <- F'(x)
      else            F[[x_id]] <- readsets[[x_id]]
  
      rel <- NA
      if(!is.null(C)) rel <- self$reads_relation(C, x, nullable)                        # Get y's related to x
      else {
        rel <- inclsets[[x_id]]
        if(is.null(rel)) rel <- list()
      }
      for(y in rel) {
        y_id <- paste(y, collapse = ' ')
        if(N[[y_id]] == 0) stack <- self$traverse(y, N, stack, F, C, nullable, readsets, inclsets)
        N[[x_id]] <- min(N[[x_id]], N[[y_id]])
        for(a in F[[y_id]]) {
          if(a %nin% F[[x_id]]) F[[x_id]][[length(F[[x_id]])+1]] <- a
        }
      }
    
      if(N[[paste(x, collapse = ' ')]] == d) {
        N[[paste(tail(stack, 1)[[1]], collapse = ' ')]] <- .Machine$integer.max
        F[[paste(tail(stack, 1)[[1]], collapse = ' ')]] <- F[[paste(x, collapse = ' ')]]
        element <- tail(stack, 1)[[1]]
        stack <- head(stack, -1)
        while(paste(element, collapse=' ') != paste(x, collapse=' ')) {
          N[[paste(tail(stack, 1)[[1]], collapse=' ')]] <- .Machine$integer.max
          F[[paste(tail(stack, 1)[[1]], collapse=' ')]] <- F[[x_id]]
          element <- tail(stack, 1)[[1]]
          stack <- head(stack, -1)
        }
      }
      
      return(stack)
    },
    # -----------------------------------------------------------------------------
    # compute_read_sets()
    #
    # Given a set of LR(0) items, this function computes the read sets.
    #
    # Inputs:  C        =  Set of LR(0) items
    #          ntrans   = Set of nonterminal transitions
    #          nullable = Set of empty transitions
    #
    # Returns a set containing the read sets
    # -----------------------------------------------------------------------------
    compute_read_sets = function(C, ntrans, nullable) {
      F <- self$digraph(ntrans, C, nullable, NULL, NULL)
      return(F)
    },
    # -----------------------------------------------------------------------------
    # compute_follow_sets()
    #
    # Given a set of LR(0) items, a set of non-terminal transitions, a readset,
    # and an include set, this function computes the follow sets
    #
    # Follow(p,A) = Read(p,A) U U {Follow(p',B) | (p,A) INCLUDES (p',B)}
    #
    # Inputs:
    #            ntrans     = Set of nonterminal transitions
    #            readsets   = Readset (previously computed)
    #            inclsets   = Include sets (previously computed)
    #
    # Returns a set containing the follow sets
    # -----------------------------------------------------------------------------
    compute_follow_sets = function(ntrans, readsets, inclsets) {
      F <- self$digraph(ntrans, NULL, NULL, readsets, inclsets)
      return(F)
    },
    # -----------------------------------------------------------------------------
    # add_lookaheads()
    #
    # Attaches the lookahead symbols to grammar rules.
    #
    # Inputs:    lookbacks         -  Set of lookback relations
    #            followset         -  Computed follow set
    #
    # This function directly attaches the lookaheads to productions contained
    # in the lookbacks set
    # -----------------------------------------------------------------------------
    add_lookaheads = function(lookbacks, followset) {
      for(trans in names(lookbacks)) {
        lb <- lookbacks[[trans]]
        # Loop over productions in lookback
        for(state_p in lb) {
          state <- state_p[[1]]
          p <- state_p[[2]]
          if(as.character(state) %nin% names(p$lookaheads)) p$lookaheads[[as.character(state)]] <- c()
          f <- followset[[trans]]
          for(a in f)
            if(a %nin% p$lookaheads[[as.character(state)]])
              p$lookaheads[[as.character(state)]] <- append(p$lookaheads[[as.character(state)]], a)
        }
      }
    },
    # -----------------------------------------------------------------------------
    # add_lalr_lookaheads()
    #
    # This function does all of the work of adding lookahead information for use
    # with LALR parsing
    # -----------------------------------------------------------------------------
    add_lalr_lookaheads = function(C) {
      # Determine all of the nullable nonterminals
      nullable <- self$compute_nullable_nonterminals()

      # Find all non-terminal transitions
      trans <- self$find_nonterminal_transitions(C)
      
      # Compute read sets
      readsets <- self$compute_read_sets(C, trans, nullable)
      
      # Compute lookback/includes relations
      lookd_included <- self$compute_lookback_includes(C, trans, nullable)
      lookd <- lookd_included[[1]]
      included <- lookd_included[[2]]

      # Compute LALR FOLLOW sets
      followsets <- self$compute_follow_sets(trans, readsets, included)
  
 		  # Add all of the lookaheads
      self$add_lookaheads(lookd, followsets)
    },
    # -----------------------------------------------------------------------------
    # lr_parse_table()
    #
    # This function constructs the parse tables for SLR or LALR
    # -----------------------------------------------------------------------------
    lr_parse_table = function() {
      Productions <- self$grammar$Productions
      Precedence  <- self$grammar$Precedence
      log <- self$log             # Logger for output
      
      actionp <- new.env(hash=TRUE)  # Action production array (temporary)
      
      log$info(sprintf('Parsing method: %s', self$lr_method))
            
      # Step 1: Construct C = { I0, I1, ... IN}, collection of LR(0) items
      # This determines the number of states
  
      C <- self$lr0_items()

      if(self$lr_method == 'LALR') self$add_lalr_lookaheads(C)
      
      # Build the parser table, state by state
      st <- 1
      for(I in C) {
        # Loop over each production in I
        actlist <- list()              # List of actions
        st_action  <- new.env(hash=TRUE)
        st_actionp <- new.env(hash=TRUE)
        st_goto    <- new.env(hash=TRUE)
        log$info('')
        log$info(sprintf('state %d', st))
        log$info('')
        for(p in I)
          log$info(sprintf('    (%d) %s', p$number, p$toString()))
        log$info('')

        for(p in I) {
          if(p$len == p$lr_index) {
            if(p$name == "S'") {
              # Start symbol. Accept!
              st_action[['$end']] <- 0
              st_actionp[['$end']] <- p
            } else {
              # We are at the end of a production.  Reduce!
              laheads <- NA
              if(self$lr_method == 'LALR') laheads <- p$lookaheads[[as.character(st)]]
              else                         laheads <- self$grammar$Follow[[p$name]]
              for(a in laheads) {
                actlist[[length(actlist)+1]] <- c(a, p, sprintf('reduce using rule %d (%s)', p$number, p$toString()))
                r <- st_action[[a]]
                if(!is.null(r)) {
                  # Whoa. Have a shift/reduce or reduce/reduce conflict
                  if(r > 0) {
                    # Need to decide on shift or reduce here
                    # By default we favor shifting. Need to add
                    # some precedence rules here.
              
                    # Shift precedence comes from the token
                    sprec_slevel <- Precedence[[a]]
                    if(is.null(rprec_rlevel)) sprec_slevel <- c('right', 0)
                    
                    sprec  <- sprec_slevel[[1]]
                    slevel <- sprec_slevel[[2]]
                    
                    # Reduce precedence comes from rule being reduced (p)
                    rprec_rlevel <- Productions[[p$number]]$prec
                    
                    rprec  <- rprec_rlevel[[1]]
                    rlevel <- rprec_rlevel[[2]]
                    
                    if((slevel < rlevel) || ((slevel == rlevel) && (rprec == 'left'))) {
                      # We really need to reduce here.
                      st_action[[a]] <- -p$number
                      st_actionp[[a]] <- p
                      if(!is.null(slevel) && !is.null(rlevel)) {
                        log$info(sprintf('  ! shift/reduce conflict for %s resolved as reduce', a))
                        self$sr_conflicts[[length(self$sr_conflicts)+1]] <- c(st, a, 'reduce')
                      }
                      Productions[[p$number]]$reduced <- Productions[[p$number]]$reduced + 1
                    } else if((slevel == rlevel) && (rprec == 'nonassoc')) {
                      st_action[[a]] <- NULL
                    } else {
                      # Hmmm. Guess we'll keep the shift
                      if(rlevel == 0) {
                        log$info('  ! shift/reduce conflict for %s resolved as shift', a)
                        self$sr_conflicts[[length(self$sr_conflicts)+1]] <- c(st, a, 'shift')
                      }
                    }
                  } else if(r < 0) {
                    # Reduce/reduce conflict.   In this case, we favor the rule
                    # that was defined first in the grammar file
                    oldp <- Productions[[-r+1]]
                    pp <- Productions[[p$number+1]]
                    if(-r+1 > p$number+1) {
                      st_action[[a]] <- -p$number
                      st_actionp[[a]] <- p
                      chosenp <- pp
                      rejectp <- oldp
                      Productions[[p$number]]$reduced <- Productions[[p$number]]$reduced + 1
                      Productions[[oldp$number]]$reduced <- Productions[[oldp$number]]$reduced - 1
                    } else {
                      chosenp <- oldp
                      rejectp <- pp
                    }
                    self$rr_conflicts[[length(self$rr_conflicts)+1]] <-c(st, chosenp, rejectp)
                    log$info(sprintf('  ! reduce/reduce conflict for %s resolved using rule %d (%s)',
                                     a, st_actionp[[a]]$number, st_actionp[[a]]$toString()))
                  } else {
                    stop(sprintf('Unknown conflict in state %d', st))
                  }
                } else {
                  st_action[[a]] <- -p$number
                  st_actionp[[a]] <- p
                  Productions[[p$number+1]]$reduced <- Productions[[p$number+1]]$reduced + 1
                }
              }
            }
          } else {
            i <- p$lr_index
            a <- p$prod[[i+1]]       # Get symbol right after the "."
            if(a %in% names(self$grammar$Terminals)) {
              g <- self$lr0_goto(I, a)
              j <- self$lr0_cidhash[[id(g)]]
              if(j >= 0) {
                # We are in a shift state
                actlist[[length(actlist)+1]] <- c(a, p, sprintf('shift and go to state %d', j))
                r <- st_action[[a]]
                if(!is.null(r)) {
                  # Whoa have a shift/reduce or shift/shift conflict
                  if(r > 0) {
                    if(r != j) stop(sprintf('Shift/shift conflict in state %d', st))
                  } else if(r < 0) {
                    # Do a precedence check.
                    #   -  if precedence of reduce rule is higher, we reduce.
                    #   -  if precedence of reduce is same and left assoc, we reduce.
                    #   -  otherwise we shift
          
                    # Shift precedence comes from the token
                    sprec_slevel <- Precedence[[a]]
                    if(is.null(sprec_slevel)) sprec_slevel <- c('right', 0)
                     
                    sprec  <- sprec_slevel[[1]]
                    slevel <- sprec_slevel[[2]]
                    
                    # Reduce precedence comes from the rule that could have been reduced
                    rprec_rlevel <- Productions[[st_actionp[[a]]$number+1]]$prec
                    
                    rprec  <- rprec_rlevel[[1]]
                    rlevel <- rprec_rlevel[[2]]
                    
                    if((slevel > rlevel) || ((slevel == rlevel) && (rprec == 'right'))) {
                      # We decide to shift here... highest precedence to shift
                      Productions[[st_actionp[[a]]$number]]$reduced <- Productions[[st_actionp[[a]]$number]]$reduced - 1
                      st_action[[a]] <- j
                      st_actionp[[a]] <- p
                      if(rlevel == 0) {
                        log$info(sprintf('  ! shift/reduce conflict for %s resolved as shift', a))
                        self$sr_conflicts[[length(self$sr_conflicts)+1]] <- c(st, a, 'shift')
                      }
                    } else if((slevel == rlevel) && (rprec == 'nonassoc')) {
                      st_action[[a]] <- NULL
                    } else {
                      # Hmmm. Guess we'll keep the reduce
                      if(slevel == 0 && rlevel == 0) {
                        log$info(sprintf('  ! shift/reduce conflict for %s resolved as reduce', a))
                        self$sr_conflicts[[length(self$sr_conflicts)+1]] <- c(st, a, 'reduce')
                      }
                    }
                  } else stop(sprintf('Unknown conflict in state %d', st))
                } else {
                  st_action[[a]] <- j
                  st_actionp[[a]] <- p
                }
              }
            }
          }
        }
        
        # Print the actions associated with each terminal
        actprint <- {}
        for(a_p_m in actlist) {
          a <- a_p_m[[1]]
          p <- a_p_m[[2]]
          m <- a_p_m[[3]]
          if(a %in% names(st_action)) {
            if(identical(p, st_actionp[[a]])) {
              log$info(sprintf('    %-15s %s', a, m))
              actprint[[paste(a, m, collapse=' ')]] <- 1
            }
          }
        }
        log$info('')
        # Print the actions that were not used. (debugging)
        not_used <- 0
        for(a_p_m in actlist) {
          a <- a_p_m[[1]]
          p <- a_p_m[[2]]
          m <- a_p_m[[3]]
          if(a %in% names(st_action)) {
            if(!identical(p, st_actionp[[a]])) {
              if(paste(a, m, collapse=' ') %nin% names(actprint)) {
                log$info(sprintf('  ! %-15s [ %s ]', a, m))
                not_used <- 1
                actprint[[paste(a, m, collapse=' ')]] <- 1
              }
            }
          }
        }
        if(not_used == 1) log$info('')
        
        # Construct the goto table for this state

        nkeys <- new.env(hash=TRUE)
        for(ii in I)
          for(s in ii$usyms)
            if(s %in% names(self$grammar$Nonterminals))
              nkeys[[s]] <- NULL
              
        for(n in names(nkeys)) {
          g <- self$lr0_goto(I, n)
          j <- self$lr0_cidhash[[id(g)]]
          if(is.null(j)) j <- -1
          if(j >= 0) {
            st_goto[[n]] <- j
            log$info(sprintf('    %-30s shift and go to state %d', n, j))
          }
        }
        self$lr_action[[as.character(st)]] <- st_action
        actionp[[as.character(st)]] <- st_actionp
        self$lr_goto[[as.character(st)]] <- st_goto
        st <- st + 1
      }
    },
    # Bind all production function names to callable objects in pdict
    bind_callables = function(instance) {
      for(p in self$lr_productions){
        p$bind(instance)
      }
    }
  ),
  private = list(
    add_count = NA
  )
)

# -----------------------------------------------------------------------------
#                            === INTROSPECTION ===
#
# The following functions and classes are used to implement the PLY
# introspection features followed by the yacc() function itself.
# -----------------------------------------------------------------------------

# Parse user's grammar
#
# This takes a raw grammar rule string and parses it into production data
# 
# @usage
# parse_grammar(name, doc)
# 
# @param name Name of the grammar method
# @param doc Rule description
# @return grammar
# 
#' @importFrom utils tail
parse_grammar = function(name, doc) {
  grammar <- list()
  # Split the doc string into lines
  pstrings <- mapply(gsub, "^\\s+|\\s+$", "", strsplit(doc, "\n"))
  lastp <- NA
  for(ps in pstrings) {
	  p <- strsplit(ps, " ")[[1]]
	  if(length(p) == 0) next
	
    prodname <- NA
    syms <- NA
    tryCatch({
  	  if(p[1] == '|') {
        # This is a continuation of a previous rule
  		  if(is.na(lastp)) stop(sprintf("%s: Misplaced '|'", name))
  		  prodname <- lastp
  		  syms <- tail(p, -1)
  	  } else {
        prodname <- p[1]
        lastp <- prodname
        syms <- tail(p, -2)
        assign <- p[2]
        if(assign != ':' && assign != '::=') stop(sprintf("%s: Syntax error. Expected ':'", name))
      }
      grammar[[length(grammar)+1]] <- list(name, prodname, syms)
    }, error = function(e) { if(startsWith(e[[1]], name)) stop(e[[1]]) 
                             else stop(sprintf("%s: Syntax error in rule %s", name, ps))})
  
  }
  return(grammar)
}


# Parser Reflect
#
# This class represents information extracted for building a parser including
# start symbol, error function, tokens, precedence list, action functions,
# etc.
#
# @docType class
# @importFrom R6 R6Class
# @format An \code{\link{R6Class}} generator object
ParserReflect <- R6Class("ParserReflect",
  public = list(
    module     = NA,
    instance   = NA,
    start      = NA,
    error_func = NA,
    tokens     = NA,
    prec       = NA,
    preclist   = NA,
    pfuncs     = NA,
    grammar    = NA,
    error      = FALSE,
    log        = NA,
    initialize = function(module, instance, log=NULL) {
      self$module <- module
      self$instance <- instance
      
      if(is.null(log)) self$log <- RlyLogger$new()
      else             self$log <- log
    },
    # Get all of the basic information
    get_all = function() {
      self$get_start()
      self$get_error_func()
      self$get_tokens()
      self$get_precedence()
      self$get_pfunctions()
    },
    # Validate all of the information
    validate_all = function() {
      self$validate_start()
      self$validate_error_func()
      self$validate_tokens()
      self$validate_precedence()
      self$validate_pfunctions()
      return(self$error)
    },
    # Get the start symbol
    get_start = function() {
      self$start <- self$instance$start
    },
    # Validate the start symbol
    validate_start = function() {
      if(!is.null(self$start))
        if(!is.character(self$start))
          self$log$error("'start' must be a string")
    },
    # Look for error handler
    get_error_func = function() {
      self$error_func <- self$instance$p_error
    },
    # Validate the error function
    validate_error_func = function() {
      if(!is.null(self$error_func)) {
        if(typeof(self$error_func) != 'closure')  {
          self$log$error("'p_error' defined, but is not a function")
          self$error <- TRUE
          return()
        }
        if(length(formals(self$error_func)) != 1) {
          self$log$error('p_error() requires 1 argument')
          self$error <- TRUE
        }
      }
    },
    # Get the tokens map
    get_tokens = function() {
      tokens <- self$instance$tokens

      if(is.null(tokens)) {
        self$log$error('No token list is defined')
        self$error <- TRUE
        return()
      }
      if(!is.vector(tokens) || typeof(tokens) != 'character') {
        self$log$error('tokens must be a vector of strings')
        self$error <- TRUE
        return()
      }
      if(length(tokens) == 0) {
        self$log$error('tokens is empty')
        self$error <- TRUE
        return()
      }

      self$tokens <- tokens
    },
    # Validate the tokens
    validate_tokens = function() {
      # Validate the tokens.
      if('error' %in% self$tokens) {
        self$log$error("Illegal token name 'error'. Is a reserved word")
        self$error <- TRUE
        return()
      }

      terminals <- list()
      for(n in self$tokens) {
        if(n %in% terminals) self$log$warn('Token %r multiply defined', n)
        terminals[length(terminals)+1] <- n
      }
    },
    # Get the precedence map (if any)
    get_precedence = function() {
      self$prec <- self$instance$precedence
    },
    # Validate and parse the precedence map
    validate_precedence = function() {
      preclist <- list()
      if(!is.null(self$prec)) {
        if(!is.list(self$prec)) {
          self$log$error("precedence must be a list")
          self$error <- TRUE
          return()
        }
        
        level <- 0
        for(p in self$prec) {
          if(!is.vector(p) || typeof(p) != 'character') {
            self$log$error("Bad precedence table")
            self$error <- TRUE
            return()
          }
          if(length(p) < 2) {
            self$log$error("Malformed precedence entry. Must be (assoc, term, ..., term)")
            self$error <- TRUE
            return()
          }
          assoc <- p[[1]]
          if(typeof(assoc) != 'character') {
            self$log$error("precedence associativity must be a string")
            self$error <- TRUE
            return()
          }
          for(term in tail(p, -1)) {
            if(typeof(term) != 'character') {
              self$log$error("precedence items must be strings")
              self$error <- TRUE
              return()
            }
            preclist[[length(preclist)+1]] <- list(term, assoc, level+1)
          }
          level <- level + 1
        }
      }
      self$preclist <- preclist
    },
    # Get all p_functions from the grammar
    get_pfunctions = function() {
      p_functions <- list()

      for(name in grep('^p_', names(self$module$public_methods), value=TRUE)) {
        if(name == 'p_error') next
        p_functions[[length(p_functions)+1]] <- name
      }
      self$pfuncs <- p_functions
    },
    # Validate all of the p_functions
    validate_pfunctions = function() {
      grammar <- list()
      # Check for non-empty symbols
      if(length(self$pfuncs) == 0) {
        self$log$error("no rules of the form p_rulename are defined")
        self$error <- TRUE
        return()
      }
      for(name in self$pfuncs) {
        f <-self$instance[[name]]
        reqargs <- 2
        nargs <- length(formals(f))
        if(nargs > reqargs) {
          self$log$error(sprintf("Rule '%s' has too many arguments", name))
          self$error <- TRUE
          return()
        }
        if(is.null(formals(f)[['doc']])) {
          self$log$error(sprintf("No documentation string specified in function '%s'", name))
          self$error <- TRUE
          return()
        }
        if(nargs < reqargs) {
          self$log$error(sprintf("Rule '%s' requires an argument", name))
          self$error <- TRUE
          return()
        }

        doc <- formals(f)[['doc']]
        tryCatch({
          parsed_g <- parse_grammar(name, doc)
          for(g in parsed_g)
            grammar[[length(grammar)+1]] <- g
        }, error = function(e) {
          self$log$error(e[[1]])
          self$error <- TRUE
        })
      }
      
      # Secondary validation step that looks for p_ definitions that are not functions
      # or functions that look like they might be grammar rules.
      for(name in names(self$instance)) {
        if(substr(name, 1, 2) == 'p_' && typeof(self$instance[[name]]) == 'closure') next
        if(substr(name, 1, 2) == 't_')                                               next
        if(substr(name, 1, 2) == 'p_' && name != 'p_error')                          self$log$warn(sprintf('%s not defined as a function', name))

        if(typeof(self$instance[[name]]) == 'closure' && length(formals(self$instance[[name]])) == 2) {
          doc <- formals(self$instance[[name]])[['doc']]
          if(!is.null(doc)) {
            d <- strsplit(doc, " ")[[1]]
            tryCatch({
                if(d[2]  == ':') self$log$warn(sprintf('%s: Possible grammar rule defined without p_ prefix', name))
            }, error = function(e) {})
          }
        }
      }
      self$grammar <- grammar
    }
  )
)


#' Build a parser
#' 
#' This function is entry point to the library
#' 
#' @param module R6 class containing rules
#' @param args list of arguments that should be passed to constructor
#' @param method type of algorithm
#' @param debug on and off debug mode
#' @param start provide custom start method
#' @param check_recursion should yacc look for recursions in rules
#' @param debugfile the name of the custom debug output logs
#' @param outputdir the dierectory of custom debug logs
#' @param debuglog custom logger for debug messages
#' @param errorlog custom logger for error messages
#' 
#' @return Parser ready to use
#' 
#' @export
#' 
#' @examples
#' TOKENS = c('NAME', 'NUMBER')
#' LITERALS = c('=','+','-','*','/', '(',')')
#' 
#' Parser <- R6Class("Parser",
#'   public = list(
#'     tokens = TOKENS,
#'     literals = LITERALS,
#'     # Parsing rules
#'     precedence = list(c('left','+','-'),
#'                       c('left','*','/'),
#'                       c('right','UMINUS')),
#'     # dictionary of names
#'     names = new.env(hash=TRUE),
#'     p_statement_assign = function(doc='statement : NAME "=" expression', p) {
#'       self$names[[as.character(p$get(2))]] <- p$get(4)
#'     },
#'     p_statement_expr = function(doc='statement : expression', p) {
#'       cat(p$get(2))
#'       cat('\n')
#'     },
#'     p_expression_binop = function(doc="expression : expression '+' expression
#'                                                   | expression '-' expression
#'                                                   | expression '*' expression
#'                                                   | expression '/' expression", p) {
#'       if(p$get(3) == '+') p$set(1, p$get(2) + p$get(4))
#'       else if(p$get(3) == '-') p$set(1, p$get(2) - p$get(4))
#'       else if(p$get(3) == '*') p$set(1, p$get(2) * p$get(4))
#'       else if(p$get(3) == '/') p$set(1, p$get(2) / p$get(4))
#'     },
#'     p_expression_uminus = function(doc="expression : '-' expression %prec UMINUS", p) {
#'       p$set(1, -p$get(3))
#'     },
#'     p_expression_group = function(doc="expression : '(' expression ')'", p) {
#'       p$set(1, p$get(3))
#'     },
#'     p_expression_number = function(doc='expression : NUMBER', p) {
#'       p$set(1, p$get(2))
#'     },
#'     p_expression_name = function(doc='expression : NAME', p) {
#'       p$set(1, self$names[[as.character(p$get(2))]])
#'     },
#'     p_error = function(p) {
#'       if(is.null(p)) cat("Syntax error at EOF")
#'       else           cat(sprintf("Syntax error at '%s'", p$value))
#'     }
#'   )
#' )
#' 
#' parser <- rly::yacc(Parser)
yacc = function(module=NA,
                args=list(),
                method='LALR',
                debug=FALSE,
                start=NA,
                check_recursion=TRUE,
                debugfile='parser.out',
                outputdir=NA,
                debuglog=NA,
                errorlog=NA) {

  if(is.na(errorlog)) errorlog <- RlyLogger$new()
              
  # Set start symbol if it's specified directly using an argument
  if(!is.na(start)) {
    module$public_methods[['start']] <- start
  }
                
  instance <- do.call("new", args, envir=module)

  # Collect parser information from the dictionary
  pinfo <- ParserReflect$new(module, instance, errorlog)
  pinfo$get_all()
  
  if(is.na(debuglog)) {
    if(debug) debuglog <- RlyLogger$new(outputdir, debugfile)
    else      debuglog <- NullLogger$new()
  }

  debuglog$info('Created by RLY (https://github.com/systemincloud/rly)')
  
  errors <- FALSE
  
  if(pinfo$validate_all()) stop('Unable to build parser')
  
  if(is.null(pinfo$error_func)) errorlog$warn('no p_error() function is defined')

  # Create a grammar object
  grammar <- Grammar$new(pinfo$tokens)

  # Set precedence level for terminals
  for(term_assoc_level in pinfo$preclist) {
    tryCatch({
      grammar$set_precedence(term_assoc_level[[1]], 
                             term_assoc_level[[2]], 
                             term_assoc_level[[3]])
    }, error = function(e) {
      errorlog$warn(e[[1]])
    })
  }
  
  # Add productions to the grammar
  for(gram in pinfo$grammar) {
    funcname <- gram[[1]]
    prodname <- gram[[2]]
    syms     <- gram[[3]]
    tryCatch({
      grammar$add_production(prodname, syms, funcname)
    }, error = function(e) {
      errorlog$error(e[[1]])
      stop('Unable to build parser')
    })
  }
  
  # Set the grammar start symbols
  if(is.na(start)) {
    if(is.null(pinfo$start)) grammar$set_start()
    else                     grammar$set_start(pinfo$start)
  } else grammar$set_start(start)
  
  if(errors) stop('Unable to build parser')
  
  # Verify the grammar structure
  undefined_symbols <- grammar$undefined_symbols()
  for(sym_prod in undefined_symbols) {
    errorlog$error(sprintf('Symbol %s used, but not defined as a token or a rule', sym_prod[[1]]))
    errors <- TRUE
  }
  
  unused_terminals <- grammar$unused_terminals()
  if(length(unused_terminals) > 0) {
    debuglog$info('')
    debuglog$info('Unused terminals:')
    debuglog$info('')
    for(term in unused_terminals) {
      errorlog$warn(sprintf('Token %s defined, but not used', term))
      debuglog$info(sprintf('    %s', term))
    }
  }

  # Print out all productions to the debug log
  if(debug) {
    debuglog$info('')
    debuglog$info('Grammar')
    debuglog$info('')
    n <- 0
    for(p in grammar$Productions) {
      debuglog$info(sprintf('Rule %-5d %s', n, p$toString()))
      n <- n + 1
    }
  }

  # Find unused non-terminals
  unused_rules <- grammar$unused_rules()
  for(prod in unused_rules) errorlog$warn(sprintf("Rule %s defined, but not used", prod$name))
  
  if(length(unused_terminals) == 1) errorlog$warn('There is 1 unused token')
  if(length(unused_terminals) > 1)  errorlog$warn(sprintf('There are %d unused tokens', length(unused_terminals)))
  if(length(unused_rules) == 1)     errorlog$warn('There is 1 unused rule')
  if(length(unused_rules) > 1)      errorlog$warn(sprintf('There are %d unused rules', length(unused_rules)))

  if(debug) {
    debuglog$info('')
    debuglog$info('Terminals, with rules where they appear')
    debuglog$info('')
	  terms <- names(grammar$Terminals)
	  terms <- sort(terms)
	  for(term in terms)
      debuglog$info(sprintf('%-20s : %s', term, paste(grammar$Terminals[[term]], sep=' ', collapse=' ')))
	  
    debuglog$info('')
    debuglog$info('Nonterminals, with rules where they appear')
    debuglog$info('')
	  nonterms <- names(grammar$Nonterminals)
	  nonterms <- sort(nonterms)
	  for(nonterm in nonterms)
      debuglog$info(sprintf('%-20s : %s', nonterm, paste(grammar$Nonterminals[[nonterm]], sep=' ', collapse=' ')))
    debuglog$info('')
  }
  
  if(check_recursion) {
    unreachable <- grammar$find_unreachable()
    for(u in unreachable) errorlog$warn(sprintf('Symbol %s is unreachable', u))
    
    infinite <- grammar$infinite_cycles()
    for(inf in infinite) {
      errorlog$error(sprintf('Infinite recursion detected for symbol %s', inf))
      errors <- TRUE
    }
  }
  
  unused_prec <- grammar$unused_precedence()
  for(term_assoc in unused_prec) {
    term  <- term_assoc[[1]]
    assoc <- term_assoc[[2]]
    errorlog$error(sprintf('Precedence rule %s defined for unknown symbol %s', assoc, term))
    errors <- TRUE
  }
  
  if(errors) stop('Unable to build parser')
  
  # Run the LRGeneratedTable on the grammar
  if(debug) cat(sprintf('Generating %s tables \n', method))

  lr <- LRGeneratedTable$new(grammar, method, debuglog)

  if(debug) {
    num_sr <- length(lr$sr_conflicts)
    # Report shift/reduce and reduce/reduce conflicts
    if     (num_sr == 1) errorlog$warn('1 shift/reduce conflict')
    else if(num_sr > 1)  errorlog$warn(sprintf('%d shift/reduce conflicts', num_sr))
    
    num_rr <- length(lr$rr_conflicts)
    if     (num_rr == 1) errorlog$warn('1 reduce/reduce conflict')
    else if(num_rr > 1)  errorlog$warn(sprintf('%d reduce/reduce conflicts', num_rr))
  }

  # Write out conflicts to the output file
  if(debug && (length(lr$sr_conflicts) > 0 || length(lr$rr_conflicts)) > 0) {
    debuglog$warn('')
    debuglog$warn('Conflicts:')
    debuglog$warn('')
    
    for(state_tok_resolution in lr$sr_conflicts) {
      state      <- state_tok_resolution[[1]]
      tok        <- state_tok_resolution[[2]]
      resolution <- state_tok_resolution[[3]]
      debuglog$warn(sprintf('shift/reduce conflict for %s in state %s resolved as %s',  tok, state, resolution))
    }
      
    already_reported <- list()
    for(state_rule_rejected in lr$rr_conflicts) {
      state    <- state_rule_rejected[[1]]
      rule     <- state_rule_rejected[[2]]
      rejected <- state_rule_rejected[[3]]
      
      if(paste(state, id(rule), id(rejected)) %in% already_reported) next
      
      debuglog$warn(sprintf('reduce/reduce conflict in state %d resolved using rule (%s)', state, rule$toString()))
      debuglog$warn(sprintf('rejected rule (%s) in state %d', rejected$toString(), state))
      errorlog$warn(sprintf('reduce/reduce conflict in state %d resolved using rule (%s)', state, rule$toString()))
      errorlog$warn(sprintf('rejected rule (%s) in state %d', rejected$toString(), state))
      already_reported[[length(already_reported)+1]] <- c(state, id(rule), id(rejected))
    }
    
    warned_never <- list()
    for(state_rule_rejected in lr$rr_conflicts) {
      state    <- state_rule_rejected[[1]]
      rule     <- state_rule_rejected[[2]]
      rejected <- state_rule_rejected[[3]]
      
      if(rejected$reduced == 0 && id(rejected) %nin% warned_never) {
        debuglog$warn(sprintf('Rule (%s) is never reduced', rejected$toString()))
        errorlog$warn(sprintf('Rule (%s) is never reduced', rejected$toString()))
        warned_never <- append(warned_never, id(rejected))
      }
    }
  }

  # Build the parser
  lr$bind_callables(instance)
  parser <- LRParser$new(lr, pinfo$error_func)

  return(parser)
}