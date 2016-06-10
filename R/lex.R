#' lex.R

#' Print debug message.
#'
#' @param msg message to display.
#' @export
dbg = function(msg) cat(c("DEBUG> ", msg, "\n"))

#' === Lexing Engine ===
#' The following Lexer class implements the lexer runtime. There are only
#' a few public methods and attributes:
#'   input()          -  Store a new string in the lexer
#'   token()          -  Get the next token
#'   clone()          -  Clone the lexer
#'
#'   lineno           -  Current line number
#'   lexpos           -  Current position in the input string
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
Lexer <- R6Class("Lexer",
  public = list(
    lexre = NA,              # Master regular expression. This is a list of
                             # tuples (re, findex) where re is a compiled
                             # regular expression and findex is a list
                             # mapping regex group numbers to rules
    lexretext = NA,          # Current regular expression strings
    lexstatere = NA,         # Dictionary mapping lexer states to master regexs
    lexstateretext = NA,     # Dictionary mapping lexer states to regex strings
    lexstaterenames = NA,    # Dictionary mapping lexer states to symbol names
    lexstate = NA,           # Current lexer state
    lexstatestack = NA,      # Stack of lexer states
    lexstateinfo = NA,       # State information
    lexstateignore = NA,     # Dictionary of ignored characters for each state
    lexstateerrorf = NA,     # Dictionary of error functions for each state
    lexstateeoff = NA,       # Dictionary of eof functions for each state
    lexreflags = NA,         # Optional re compile flags
    lexdata = NA,            # Actual input data (as a string)
    lexpos = NA,             # Current position in input text
    lexlen = NA,             # Length of the input text
    lexerrorf = NA,          # Error rule (if any)
    lexeoff = NA,            # EOF rule (if any)
    lextokens = NA,          # List of valid tokens
    lexignore = NA,          # Ignored characters
    lexliterals = NA,        # Literal characters that can be passed through
    lexmodule = NA,          # Module
    lineno = NA,             # Current line number
    lexoptimize = NA,        # Optimized mode

    initialize = function() {
      self$lexre <- NA
      self$lexretext <- NA
      self$lexstatere <- new.env(hash=TRUE)
      self$lexstateretext <- new.env(hash=TRUE)
      self$lexstaterenames <- new.env(hash=TRUE)
      self$lexstate <- 'INITIAL'
      self$lexstatestack <- c()
      self$lexstateinfo <- NA
      self$lexstateignore <- new.env(hash=TRUE)
      self$lexstateerrorf <- new.env(hash=TRUE)
      self$lexstateeoff <- new.env(hash=TRUE)
      self$lexreflags <- 0
      self$lexdata <- NA
      self$lexpos <- 0
      self$lexlen <- 0
      self$lexerrorf <- NA
      self$lexeoff <- NA
      self$lextokens <- NA
      self$lexignore <- ''
      self$lexliterals <- ''
      self$lexmodule <- NA
      self$lineno <- 1
      self$lexoptimize <- FALSE
    },
    #' ------------------------------------------------------------
    #' writetab() - Write lexer information to a table file
    #' ------------------------------------------------------------
    writetab = function(lextab, outputdir='') {
    },
    #' ------------------------------------------------------------
    #' readtab() - Read lexer information from a tab file
    #' ------------------------------------------------------------
    readtab = function(self, tabfile, fdict) {
    },
    #' ------------------------------------------------------------
    #' input() - Push a new string into the lexer
    #' ------------------------------------------------------------
    input = function(s) {

    },
    #' ------------------------------------------------------------
    #' begin() - Changes the lexing state
    #' ------------------------------------------------------------
    begin = function(state) {
    },
    #' ------------------------------------------------------------
    #' push_state() - Changes the lexing state and saves old on stack
    #' ------------------------------------------------------------
    push_state = function(state) {
    },
    #' ------------------------------------------------------------
    #' pop_state() - Restores the previous state
    #' ------------------------------------------------------------
    pop_state = function(self) {
    },
    #' ------------------------------------------------------------
    #' current_state() - Returns the current lexing state
    #' ------------------------------------------------------------
    current_state = function() {
    },
    #' ------------------------------------------------------------
    #' skip() - Skip ahead n characters
    #' ------------------------------------------------------------
    skip = function(n) {
    },
    #' ------------------------------------------------------------
    #' opttoken() - Return the next token from the Lexer
    #' ------------------------------------------------------------
    token = function() {
      # Make local copies of frequently referenced attributes
      lexpos    <- self$lexpos
      lexlen    <- self$lexlen
      lexignore <- self$lexignore
      lexdata   <- self$lexdata

      while(lexpos < lexlen) {
        # This code provides some short-circuit code for whitespace, tabs, and other ignored characters
        if(lexdata[lexpos] %in% lexignore) {
          lexpos = lexpos + 1
          next
        }

        broke <- FALSE

        # Look for a regular expression match
        for(lexre in self$lexre) {
          m <- lexre$match(lexdata, lexpos)
          if(!m) next

          # Create a token for return
          tok <- LexToken$new()
          tok$value <- m$group()
          tok$lineno <- self$lineno
          tok$lexpos <- lexpos

          i <- m$lastindex
          func <- lexindexfunc[i]

          if(!func) {
            # If no token type was set, it's an ignored token
            if(tok$type) {
              self$lexpos <- m$end()
              return(tok)
            } else {
              lexpos <- m$end()
              broke <- TRUE
              break
            }
          }

          lexpos <- m$end()

          # If token is processed by a function, call it

          tok$lexer <- self      # Set additional attributes useful in token rules
          self$lexmatch <- m
          self$lexpos <- lexpos

          newtok <- func(tok)

          # Every function must return a token, if nothing, we just move to next token
          if(!newtok) {
            lexpos    <- self.lexpos         # This is here in case user has updated lexpos.
            lexignore <- self.lexignore      # This is here in case there was a state change
            broke <- TRUE
            break
          }

          # Verify type of the token.  If not in the token map, raise an error
          if(self$lexoptimize) {
            if(!(newtok$type %in% self$lextokens_all)) {
              stop(sprintf("%s:%d: Rule '%s' returned an unknown token type '%s'", func$code$co_filename,
                                                                                   func$name,
                                                                                   newtok$type))
            }
          }
          return(newtok)
        }
        if(!broke) {
          # No match, see if in literals
          if(lexdata[lexpos] %in% self$lexliterals) {
            tok <- LexToken()
            tok$value <- lexdata[lexpos]
            tok$lineno <- self$lineno
            tok$type <- tok$value
            tok$lexpos <- lexpos
            self$lexpos <- lexpos + 1
            return(tok)
          }

          # No match. Call t_error() if defined.
          if(self$lexerrorf) {
            tok <- LexToken$new()
            tok$value <- head(self$lexdata, lexpos)
            tok$lineno <- self$lineno
            tok$type <- 'error'
            tok$lexer <- self
            tok$lexpos <- lexpos
            self$lexpos <- lexpos
            newtok <- self$lexerrorf(tok)
            if(lexpos == self.lexpos) {
              # Error method didn't change text position at all. This is an error.
              stop(sprintf("Scanning error. Illegal character '%s'", lexdata[lexpos]))
            }
            lexpos <- self.lexpos
            if(!newtok) continue
            return(newtok)
          }

          self.lexpos <- lexpos
          stop(sprintf("Illegal character '%s' at index %d", lexdata[lexpos], lexpos))
        }
      }
#          formals(g)$y

      if(self$lexeoff) {
        tok <- LexToken$new()
        tok$type <- 'eof'
        tok$value <- ''
        tok$lineno <- self.lineno
        tok$lexpos <- lexpos
        tok$lexer <- self
        self$lexpos <- lexpos
        newtok <- self$lexeoff(tok)
        return(newtok)
      }

      self$lexpos = lexpos + 1
      if(is.na(self$lexdata)) stop('No input string given with input()')
    }
  )
)


#' Build all of the regular expression rules from definitions in the supplied module
#' @export
lex = function(module=NA,
               debug=FALSE,
               reflags=0,
               nowarn=FALSE,
               outputdir=NA) {
  ldict <- NA
  stateinfo <- new.env(hash=TRUE)
  stateinfo[['INITIAL']] <- 'inclusive'
  lexobj = Lexer$new()

  items <- ls(module)

  return(lexobj)
}
