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

# This regular expression is used to match valid token names
reg_is_identifier = '^[a-zA-Z0-9_]+$'


#' Lex Token
#'
#' Token class.  This class is used to represent the tokens produced
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#'
#' @export
LexToken <- R6::R6Class("LexToken",
  public = list(
    type   = NA,
    value  = NA,
    lineno = NA,
    lexpos = NA,
    lexer  = NA,
    parser = NA,
    toString = function() {
      return(sprintf("LexToken(%s,%s,%d,%d)", self$type, toString(self$value), self$lineno, self$lexpos))
    },
    print = function(...) {
      cat(sprintf("LexToken(%s,%s,%d,%d)\n", self$type, toString(self$value), self$lineno, self$lexpos))
      invisible(self)
    }
  )
)


#' Lexing Engine
#'
#' @description {
#' The following Lexer class implements the lexer runtime. There are only
#' a few public methods and attributes:
#'
#' \itemize{
#'  \item input() - Store a new string in the lexer
#'  \item token() - Get the next token
#'  \item clone() - Clone the lexer
#'
#'  \item lineno  - Current line number
#'  \item lexpos  - Current position in the input string
#' }
#' }
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
Lexer <- R6::R6Class("Lexer",
  public = list(
    instance        = NA,
    lexre           = NA, # Master regular expression. This is a list of
                          # tuples (re, findex) where re is a compiled
                          # regular expression and findex is a list
                          # mapping regex group numbers to rules
    lexretext       = NA, # Current regular expression strings
    lexstatere      = NA, # Dictionary mapping lexer states to master regexs
    lexstateretext  = NA, # Dictionary mapping lexer states to regex strings
    lexstaterenames = NA, # Dictionary mapping lexer states to symbol names
    lexstate        = NA, # Current lexer state
    lexstatestack   = NA, # Stack of lexer states
    lexstateinfo    = NA, # State information
    lexstateignore  = NA, # Dictionary of ignored characters for each state
    lexstateerrorf  = NA, # Dictionary of error functions for each state
    lexstateeoff    = NA, # Dictionary of eof functions for each state
    lexdata         = NA, # Actual input data (as a string)
    lexpos          = NA, # Current position in input text
    lexlen          = NA, # Length of the input text
    lexerrorf       = NA, # Error rule (if any)
    lexeoff         = NA, # EOF rule (if any)
    lextokens       = NA, # List of valid tokens
    lextokens_all   = NA,
    lexignore       = NA, # Ignored characters
    lexliterals     = NA, # Literal characters that can be passed through
    lexmodule       = NA, # Module
    lineno          = NA, # Current line number
    lexoptimize     = NA, # Optimized mode

    initialize = function(instance) {
      self$instance        <- instance
      self$lexre           <- NA
      self$lexretext       <- NA
      self$lexstatere      <- new.env(hash=TRUE)
      self$lexstateretext  <- new.env(hash=TRUE)
      self$lexstaterenames <- new.env(hash=TRUE)
      self$lexstate        <- 'INITIAL'
      self$lexstatestack   <- c()
      self$lexstateinfo    <- NA
      self$lexstateignore  <- new.env(hash=TRUE)
      self$lexstateerrorf  <- new.env(hash=TRUE)
      self$lexstateeoff    <- new.env(hash=TRUE)
      self$lexdata         <- NA
      self$lexpos          <- 1
      self$lexlen          <- 0
      self$lexerrorf       <- NULL
      self$lexeoff         <- NULL
      self$lextokens       <- NA
      self$lexignore       <- c()
      self$lexliterals     <- NULL
      self$lexmodule       <- NA
      self$lineno          <- 1
      self$lexoptimize     <- FALSE
    },
    # ------------------------------------------------------------
    # input() - Push a new string into the lexer
    # ------------------------------------------------------------
    input = function(s) {
      if(!is.character(s)) stop('[ValueError]', 'Expected a string')
      self$lexdata <- s
      self$lexpos  <- 1
      self$lexlen  <- nchar(s)
    },
    # ------------------------------------------------------------
    # begin() - Changes the lexing state
    # ------------------------------------------------------------
    begin = function(state) {
      if(!(state %in% names(self$lexstatere))) stop('[ValueError]', 'Undefined state')
      self$lexre     <- self$lexstatere[[state]]
      self$lexretext <- self$lexstateretext[[state]]
      self$lexignore <- self$lexstateignore[[state]]
      self$lexerrorf <- self$lexstateerrorf[[state]]
      self$lexeoff   <- self$lexstateeoff[[state]]
      self$lexstate  <- state
    },
    # ------------------------------------------------------------
    # push_state() - Changes the lexing state and saves old on stack
    # ------------------------------------------------------------
    push_state = function(state) {
      self$lexstatestack <- append(self$lexstatestack, self$lexstate)
      self$begin(state)
    },
    # ------------------------------------------------------------
    # pop_state() - Restores the previous state
    # ------------------------------------------------------------
    pop_state = function() {
      state <- tail(self$lexstatestack, 1)
      self$lexstatestack <- head(self$lexstatestack, -1)
      self$begin(state)
    },
    # ------------------------------------------------------------
    # current_state() - Returns the current lexing state
    # ------------------------------------------------------------
    current_state = function() {
      return(self$lexstate)
    },
    # ------------------------------------------------------------
    # skip() - Skip ahead n characters
    # ------------------------------------------------------------
    skip = function(n) {
      self$lexpos <- self$lexpos + n
    },
    # ------------------------------------------------------------
    # opttoken() - Return the next token from the Lexer
    # ------------------------------------------------------------
    token = function() {
      # Make local copies of frequently referenced attributes
      lexpos    <- self$lexpos
      lexlen    <- self$lexlen
      lexignore <- self$lexignore
      lexdata   <- self$lexdata

      while(lexpos <= lexlen) {
        # This code provides some short-circuit code for whitespace, tabs, and other ignored characters
        if(length(lexignore) > 0)
          if(grepl(substring(lexdata, lexpos, lexpos), lexignore, fixed=TRUE)) {
            lexpos <- lexpos + 1
            next
          }

        broke <- FALSE

        data <- substring(lexdata, lexpos)
        # Look for a regular expression match
        tok <- NA
        idx <- 0
        for(lexre in self$lexre) {
          name <- lexre[[1]]
          regx <- lexre[[2]]
          func <- lexre[[3]]
          type <- lexre[[4]]
          m <- regexpr(regx, data, perl=TRUE)
          if(m != 1) next

          matched <- regmatches(data, m)

          # Create a token for return
          tok <- LexToken$new()
          tok$value <- matched
          tok$lineno <- self$lineno
          tok$lexpos <- lexpos
          tok$type <- type

          if(is.null(func)) {
            # If no token type was set, it's an ignored token
            if(!is.null(tok$type)) {
              self$lexpos <- lexpos + nchar(matched)
              return(tok)
            } else {
              lexpos <- lexpos + nchar(matched)
              broke <- TRUE
              break
            }
          }

          lexpos <- lexpos + nchar(toString(matched))

          # If token is processed by a function, call it

          tok$lexer   <- self      # Set additional attributes useful in token rules
          self$lexpos <- lexpos

          newtok <- func(t=tok)
          # Every function must return a token, if nothing, we just move to next token
          if(is.null(newtok)) {
            lexpos    <- self$lexpos         # This is here in case user has updated lexpos.
            lexignore <- self$lexignore      # This is here in case there was a state change
            broke <- TRUE
            break
          }

          # Verify type of the token.  If not in the token map, raise an error
          if(!(newtok$type %in% self$lextokens_all)) stop('[LexError]', sprintf("Rule '%s' returned an unknown token type '%s'", name, newtok$type))

          return(newtok)
        }

        if(!broke) {
          # No match, see if in literals
          if(!is.null(self$lexliterals)) {
            if(grepl(substring(data, 1, 1), self$lexliterals, fixed=TRUE)) {
              tok         <- LexToken$new()
              tok$value   <- substring(data, 1, 1)
              tok$lineno  <- self$lineno
              tok$type    <- tok$value
              tok$lexpos  <- lexpos
              self$lexpos <- lexpos + 1
              return(tok)
            }
          }

          # No match. Call t_error() if defined.
          if(!is.null(self$lexerrorf)) {
            tok <- LexToken$new()
            tok$value <- substring(data, 1, 1)
            tok$lineno <- self$lineno
            tok$type <- 'error'
            tok$lexer <- self
            tok$lexpos <- lexpos
            self$lexpos <- lexpos
            newtok <- self$lexerrorf(tok)
            if(lexpos == self$lexpos) {
              # Error method didn't change text position at all. This is an error.
              stop('[LexError]', sprintf("Scanning error. Illegal character '%s'", substring(data, 1, 1)))
            }
            lexpos <- self$lexpos
            if(is.null(newtok)) next
            return(newtok)
          }

          self.lexpos <- lexpos
          stop('[LexError]', sprintf("Illegal character '%s' at index %d", substring(data, 1, 1), lexpos))
        }
      }

      if(!is.null(self$lexeoff)) {
        tok <- LexToken$new()
        tok$type <- 'eof'
        tok$value <- ''
        tok$lineno <- self$lineno
        tok$lexpos <- lexpos
        tok$lexer <- self
        self$lexpos <- lexpos
        newtok <- self$lexeoff(tok)
        return(newtok)
      }

      self$lexpos <- lexpos + 1
      if(is.na(self$lexdata)) stop('[RuntimeError]', 'No input string given with input()')
    }
    # Iterator interface
    # TODO
  )
)


# Get regex from function
#
# Returns the regular expression assigned to a function.
#
# @param func lexer function
#
# @return regex
get_regex = function(func) {
  return(formals(func)[['re']])
}


# State-Token tuple
#
# Given a declaration name s of the form "t_" and a dictionary whose keys are
# state names, this function returns a tuple (states,tokenname) where states
# is a tuple of state names and tokenname is the name of the token.  For example,
# calling this with s = "t_foo_bar_SPAM" might return (('foo','bar'),'SPAM')
#
# @param s rule name
# @param names state names
#
# @return (states,tokenname) tuple
#
#' @importFrom utils head
statetoken = function(s, names) {
  parts <- unlist(strsplit(s, "_", fixed=TRUE))

  i <- 0
  for(part in tail((parts), -1L)) {
    i <- i + 1
    if(!(part %in% names(names)) && (part != 'ANY')) break
  }

  if(i > 1) states <- head(tail((parts), -1L), i - 1)
  else      states <- c('INITIAL')

  if('ANY' %in% states) states <- c(names(names))

  tokenname <- paste(tail(parts, -i), collapse='_')
  return(list(states, tokenname))
}


# Lexer Reflect
#
# This class represents information needed to build a lexer as extracted from a
# user's input file.
#
# @docType class
# @importFrom R6 R6Class
# @format An \code{\link{R6Class}} generator object
LexerReflect <- R6::R6Class("LexerReflect",
  public = list(
    module    = NA,
    instance  = NA,
    tokens    = NA,
    literals  = NA,
    states    = NA,
    stateinfo = NA,
    error     = FALSE,
    log       = NA,

    toknames = NA,        # Mapping of symbols to token names
    funcsym  = NA,        # Symbols defined as functions
    strsym   = NA,        # Symbols defined as strings
    ignore   = NA,        # Ignore strings by state
    errorf   = NA,        # Error functions by state
    eoff     = NA,        # EOF functions by state

    initialize = function(module, instance, log=NULL) {
      self$module <- module
      self$instance <- instance
      self$tokens <- c()
      self$literals <- c()
      self$states <- c()
      self$stateinfo <- new.env()
      self$stateinfo[['INITIAL']] <- 'inclusive'
      if(is.null(log)) self$log <- RlyLogger$new()
      else             self$log <- log
    },
    # Get all of the basic information
    get_all = function() {
      self$get_tokens()
      self$get_literals()
      self$get_states()
      self$get_rules()
    },
    # Validate all of the information
    validate_all = function() {
      self$validate_tokens()
      self$validate_literals()
      self$validate_rules()
      return(self$error)
    },
    # Get the tokens map
    get_tokens = function() {
      tokens <- self$instance$tokens
      if(is.null(tokens)) {
        self$log$error('No token list is defined')
        self$error <- TRUE
        return()
      }
      if(!is.vector(tokens)) {
        self$log$error('tokens must be a vector')
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
      terminals = c()
      for(t in self$tokens) {
        if(!grepl(reg_is_identifier, t, perl=TRUE)) {
          self$log$error(sprintf("Bad token name '%s'", t))
          self$error <- TRUE
        }
        if(t %in% terminals) self$log$warn(sprintf("Token '%s' multiply defined", t))
        terminals <- c(terminals, t)
      }
    },
    # Get the literals specifier
    get_literals = function() {
      literals <- self$instance$literals
      if(is.null(literals)) literals <- c()
      if(length(literals) == 1 && typeof(literals) == "character")
        if(nchar(literals) > 1)
          literals <- strsplit(literals, '')
      self$literals <- literals
    },
    # Validate literals
    validate_literals = function() {
      for(l in self$literals) {
        if(!is.character(l) || nchar(l) > 1) {
          self$log$error('Invalid literal. Must be a single character')
          self$error <- TRUE
        }
      }
    },
    get_states = function() {
      self$states = self$instance$states
      # Build statemap
      if(!is.null(self$states)) {
        if(!is.vector(self$states)) {
          self$log$error('states must be defined as a vector')
          self$error <- TRUE
        } else {
          for(s in self$states) {
            if(!is.vector(s) || length(s) != 2) {
              self$log$error("Invalid state specifier. Must be a tuple (statename,'exclusive|inclusive')")
              self$error <- TRUE
              next
            }
            name      <- s[1]
            statetype <- s[2]

            if(!is.character(name)) {
              self$log$error('State name must be a string')
              self$error <- TRUE
              next
            }
            if(!(statetype == 'inclusive' || statetype == 'exclusive')) {
              self$log$error("State type for state must be 'inclusive' or 'exclusive'")
              self$error <- TRUE
              next
            }
            if(name %in% names(self$stateinfo)) {
              self$log$error(sprintf("State '%s' already defined", name))
              self$error <- TRUE
              next
            }

            self$stateinfo[[name]] <- statetype
          }
        }
      }
    },
    # Get all of the symbols with a t_ prefix and sort them into various
    # categories (functions, strings, error functions, and ignore characters)
    get_rules = function() {
      tsymbols <- c(grep('^t_', names(self$module$public_fields), value=TRUE),
                    grep('^t_', names(self$module$public_methods), value=TRUE))

      # Now build up a list of functions and a list of strings
      self$toknames <- new.env()
      self$funcsym  <- new.env()
      self$strsym   <- new.env()
      self$ignore   <- new.env()
      self$errorf   <- new.env()
      self$eoff     <- new.env()

      for(s in names(self$stateinfo)) {
        self$funcsym[[s]] <- list()
        self$strsym[[s]] <- list()
      }

      if(length(tsymbols) == 0) {
        self$log$error('No rules of the form t_rulename are defined')
        self$error <- TRUE
        return()
      }

      for(f in tsymbols) {
        t <- self$instance[[f]]
        st <- statetoken(f, self$stateinfo)
        states <- st[1]
        tokname <- st[[2]]
        self$toknames[[f]] <- tokname

        if(typeof(t) == 'closure') {
          if(tokname == 'error')       for(s in states) self$errorf[[s]] <- t
          else if(tokname == 'eof')    for(s in states) self$eoff[[s]]   <- t
          else if(tokname == 'ignore') {
            self$log$error(sprintf("Rule '%s' must be defined as a string", f))
            self$error <- TRUE
          } else                         for(s in states) self$funcsym[[s]][[length(self$funcsym[[s]])+1]] <- list(f, t)
        } else if(typeof(t) == 'character') {
          if(tokname == 'ignore') {
            for(s in states) self$ignore[[s]] <- t
            if('\\' %in% t) self$log$warn("%s contains a literal backslash '\\'", f)
          } else if(tokname == 'error') {
            self$log$error(sprintf("Rule '%s' must be defined as a function", f))
            self$error <- TRUE
          } else for(s in states) self$strsym[[s]][[length(self$strsym[[s]])+1]] <- list(f, t)
        } else {
          self$log$error(sprintf('%s not defined as a function or string', f))
          self$error <- TRUE
        }
      }
    },
    # Validate all of the t_rules collected
    validate_rules = function() {
      for(state in names(self$stateinfo)) {

        # Validate all rules defined by functions
        for(ft in self$funcsym[[state]]) {
          fname <- ft[[1]]
          f <- ft[[2]]
          tokname <- self$toknames[[fname]]
          reqargs <- 2
          nargs <- length(formals(f))
          if(nargs > reqargs) {
            self$log$error(sprintf("Rule '%s' has too many arguments", fname))
            self$error <- TRUE
            next
          }
          if(nargs < reqargs) {
            self$log$error(sprintf("Rule '%s' requires an argument", fname))
            self$error <- TRUE
            next
          }
          if(is.null(get_regex(f))) {
            self$log$error(sprintf("No regular expression defined for rule '%s'", fname))
            self$error <- TRUE
            next
          }
          tryCatch({
          	if(grepl(get_regex(f), '', perl=TRUE)) {
              self$log$error(sprintf("Regular expression for rule '%s' matches empty string", fname))
              self$error <- TRUE
            }
          }, error = function(e) {
            self$log$error(sprintf("Invalid regular expression for rule '%s'", name))
            self$error <- TRUE
          })
        }

        # Validate all rules defined by strings
        for(nr in self$strsym[[state]]) {
          name <- nr[[1]]
          r <- nr[[2]]
          tokname <- self$toknames[[name]]
          if(tokname == 'error') {
            self$log$error(sprintf("Rule '%s' must be defined as a function", name))
            self$error <- TRUE
            next
          }
          if(!(tokname %in% self$tokens) && !grepl(tokname, 'ignore_')) {
            self$log$error(sprintf("Rule '%s' defined for an unspecified token %s", name, tokname))
            self$error <- TRUE
            next
          }
          tryCatch({
          	if(grepl(r, '', perl=TRUE)) {
              self$log$error(sprintf("Regular expression for rule '%s' matches empty string", name))
              self$error <- TRUE
            }
          }, error = function(e) {
            self$log$error(sprintf("Invalid regular expression for rule '%s'", name))
            self$error <- TRUE
          })
        }

        if(length(self$funcsym[[state]]) == 0 && length(self$strsym[[state]]) == 0) {
          self$log$error(sprintf("No rules defined for state '%s'", state))
          self$error <- TRUE
        }

        # Validate the error function
        efunc <- self$errorf[[state]]
        if(!is.null(efunc)) {
          f <- efunc
          reqargs <- 1
          nargs <- length(formals(f))
          if(nargs > reqargs) {
            self$log$error("Rule error has too many arguments")
            self$error <- TRUE
          }
          if(nargs < reqargs) {
            self$log$error("Rule error requires an argument")
            self$error <- TRUE
          }
        }
      }
    }
  )
)


#' Build a lexer
#'
#' Build all of the regular expression rules from definitions in the supplied module
#'
#' @param module R6 class containing lex rules
#' @param args list of arguments that should be passed to constructor
#' @param debug on and off debug mode
#' @param debuglog custom logger for debug messages
#' @param errorlog custom logger for error messages
#'
#' @return Lexer ready to use
#'
#' @export
#'
#' @examples
#' TOKENS = c('NAME', 'NUMBER')
#' LITERALS = c('=','+','-','*','/', '(',')')
#'
#' Lexer <- R6::R6Class("Lexer",
#'   public = list(
#'     tokens = TOKENS,
#'     literals = LITERALS,
#'     t_NAME = '[a-zA-Z_][a-zA-Z0-9_]*',
#'     t_NUMBER = function(re='\\d+', t) {
#'       t$value <- strtoi(t$value)
#'       return(t)
#'     },
#'     t_ignore = " \t",
#'     t_newline = function(re='\\n+', t) {
#'        t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
#'        return(NULL)
#'     },
#'     t_error = function(t) {
#'       cat(sprintf("Illegal character '%s'", t$value[1]))
#'       t$lexer$skip(1)
#'       return(t)
#'     }
#'   )
#' )
#'
#' lexer  <- rly::lex(Lexer)
lex = function(module=NA,
               args=list(),
               debug=FALSE,
               debuglog=NA,
               errorlog=NA) {
  instance <- do.call("new", args, envir=module)
  lexobj <- Lexer$new(instance)

  if(is.na(errorlog)) errorlog <- RlyLogger$new()

  if(debug) {
    if(is.na(debuglog)) debuglog <- RlyLogger$new()
  }

  # Collect parser information
  linfo = LexerReflect$new(module, instance, log=errorlog)
  linfo$get_all()
  if(linfo$validate_all()) stop('[SyntaxError]', "Can't build lexer")

  # Dump some basic debugging information
  if(debug) {
    if(length(linfo$tokens) > 0)    debuglog$info(sprintf('lex: tokens   = %s', paste(linfo$tokens, collapse=" ")))
    else                            debuglog$info('lex: tokens empty')
    if(length(linfo$literals) > 0)  debuglog$info(sprintf('lex: literals = %s', paste(linfo$literals, collapse=" ")))
    else                            debuglog$info('lex: literals empty')
    if(length(linfo$stateinfo) > 0) debuglog$info(sprintf('lex: states   = %s',
                                                  paste('{',
                                                        sapply(names(linfo$stateinfo), function(x) paste("'", x, "'", " : ", "'", linfo$stateinfo[[x]], "', ", collapse='', sep='')),
                                                        '}', collapse=" ")))
    else                            debuglog$info('lex: states empty')
  }

  # Build a dictionary of valid token names
  lexobj$lextokens <- unique(linfo$tokens)

  # Get literals specification
  if(is.vector(linfo$literals)) lexobj$lexliterals <- paste(linfo$literals, collapse = "")
  else                          lexobj$lexliterals <- linfo$literals

  lexobj$lextokens_all <- c(lexobj$lextokens, unique(lexobj$lexliterals))

  # Get the stateinfo dictionary
  stateinfo <- linfo$stateinfo

  regexs <- new.env()
  # Build the master regular expressions
  for(state in names(stateinfo)) {
    regex_list <- list()

    # Add rules defined by functions first
    for(fnamef in linfo$funcsym[[state]]) {
      fname <- fnamef[[1]]
      f <- fnamef[[2]]
      regex_list[[length(regex_list)+1]] <- list(fname, get_regex(f), f, linfo$toknames[[fname]])
      if(debug) debuglog$info(sprintf("lex: Adding rule %s -> '%s' (state '%s')", fname, get_regex(f), state))
    }

    # Now add all of the simple rules
    for(namer in linfo$strsym[[state]]) {
      name <- namer[[1]]
      r <- namer[[2]]
      toktype <- if(grepl("ignore_", name)) NULL else linfo$toknames[[name]]
      regex_list[[length(regex_list)+1]] <- list(name, r, NULL, toktype)
      if(debug) debuglog$info(sprintf("lex: Adding rule %s -> '%s' (state '%s')", name, r, state))
    }

    regexs[[state]] <- regex_list
  }

  # Build the master regular expressions

  if(debug) debuglog$info('lex: ==== MASTER REGEXS FOLLOW ====')

  for(state in names(regexs)) {
    lexobj$lexstatere[[state]] <- regexs[[state]]
    if(debug) {
      for(nr in regexs[[state]]) {
        debuglog$info(sprintf("lex: state '%s' : regex = '%s'", state, nr[2]))
      }
    }
  }

  # For inclusive states, we need to add the regular expressions from the INITIAL state
  for(state in names(stateinfo)) {
    stype <- stateinfo[[state]]
    if(state != 'INITIAL' && stype == 'inclusive') {
      lexobj$lexstatere[[state]] <- list(lexobj$lexstatere[[state]], lexobj$lexstatere[['INITIAL']])
    }
  }

  lexobj$lexstateinfo <- stateinfo
  lexobj$lexre <- lexobj$lexstatere[['INITIAL']]

  # Set up ignore variables
  lexobj$lexstateignore <- linfo$ignore
  lexobj$lexignore <- lexobj$lexstateignore[['INITIAL']]

  # Set up error functions
  lexobj$lexstateerrorf <- linfo$errorf
  lexobj$lexerrorf <- linfo$errorf[['INITIAL']]
  if(is.null(lexobj$lexerrorf)) errorlog$warn('No t_error rule is defined')

  # Set up eof functions
  lexobj$lexstateeoff <- linfo$eoff
  lexobj$lexeoff = linfo$eoff[['INITIAL']]

  # Check state information for ignore and error rules
  for(s in names(stateinfo)) {
    stype <- stateinfo[[s]]
    if(stype == 'exclusive') {
      if(!(s %in% names(linfo$errorf)))                                      errorlog$warn(sprintf("No error rule is defined for exclusive state '%s'", s))
      if(!(s %in% names(linfo$ignore)) && !(s %in% names(lexobj$lexignore))) errorlog$warn(sprintf("No ignore rule is defined for exclusive state '%s'", s))
    } else if(stype == 'inclusive') {
      if(!(s %in% names(linfo$errorf))) {
        linfo$errorf[[s]] <- linfo$errorf[['INITIAL']]
        if(!(s %in% names(linfo$ignore))) linfo$ignore[[s]] <- linfo$ignore[['INITIAL']]
      }
    }
  }

  # If in optimize mode, we write the lextab
  # TODO

  return(lexobj)
}
