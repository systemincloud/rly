#' lex.R

#' Print debug message.
#'
#' @param msg message to display.
#' @export
dbg = function(msg) cat(c("DEBUG> ", msg, "\n"))

#' This regular expression is used to match valid token names
reg_is_identifier = '^[a-zA-Z0-9_]+$'

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

      self$lexpos <- lexpos + 1
      if(is.na(self$lexdata)) stop('No input string given with input()')
    }
  )
)


#' LexerReflect
#' This class represents information needed to build a lexer as extracted from a
#' user's input file.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
LexerReflect <- R6Class("LexerReflect",
  public = list(
    module = NA,
    tokens = NA,
    literals = NA,
    states = NA,
    stateinfo = NA,
    error = NA,
    initialize = function(module) {
      self$module <- module
      self$tokens <- c()
      self$literals <- c()
      self$states <- c()
      self$stateinfo <- c()
      self$error <- FALSE
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
      tokens <- self$module$tokens
      if(is.null(tokens)){
        dbg('No token list is defined')
        self$error = TRUE
        return
      }
      if(!is.vector(tokens)) {
        dbg('tokens must be a vector')
        self$error = TRUE
        return
      }
      if(length(tokens) == 0) {
        dbg('tokens is empty')
        self$error = TRUE
        return
      }
      self$tokens <- tokens
    },
    # Validate the tokens
    validate_tokens = function() {
      terminals = c()
      for(t in self$tokens) {
        if(!grepl(reg_is_identifier, t, perl=TRUE)) {
          dbg(sprintf("Bad token name '%s'", t))
      		self$error = TRUE
        }
        if(t %in% terminals) dbg(sprintf("Token '%s' multiply defined", t))
        terminals = c(terminals, t)
      }
    },
    # Get the literals specifier
    get_literals = function() {
#      self.literals = self.ldict.get('literals', '')
#      if not self.literals:
#            self.literals = ''
    },
    # Validate literals
    validate_literals = function(){
#      try:
#          for c in self.literals:
#          if not isinstance(c, StringTypes) or len(c) > 1:
#                self.log.error('Invalid literal %s. Must be a single character', repr(c))
#      self.error = True
#
#      except TypeError:
#          self.log.error('Invalid literals specification. literals must be a sequence of characters')
#      self.error = True
    },
    get_states = function() {
#      self.states = self.ldict.get('states', None)
## Build statemap
#      if self.states:
#            if not isinstance(self.states, (tuple, list)):
#        self.log.error('states must be defined as a tuple or list')
#    self.error = True
#    else:
#              for s in self.states:
#        if not isinstance(s, tuple) or len(s) != 2:
#              self.log.error("Invalid state specifier %s. Must be a tuple (statename,'exclusive|inclusive')", repr(s))
#    self.error = True
#    continue
#    name, statetype = s
#    if not isinstance(name, StringTypes):
#          self.log.error('State name %s must be a string', repr(name))
#    self.error = True
#    continue
#    if not (statetype == 'inclusive' or statetype == 'exclusive'):
#          self.log.error("State type for state %s must be 'inclusive' or 'exclusive'", name)
#    self.error = True
#    continue
#    if name in self.stateinfo:
#          self.log.error("State '%s' already defined", name)
#    self.error = True
#    continue
#    self.stateinfo[name] = statetype
    },
    # Get all of the symbols with a t_ prefix and sort them into various
    # categories (functions, strings, error functions, and ignore characters)
    get_rules = function() {
#      tsymbols = [f for f in self.ldict if f[:2] == 't_']
#
## Now build up a list of functions and a list of strings
#      self.toknames = {}        # Mapping of symbols to token names
#      self.funcsym  = {}        # Symbols defined as functions
#      self.strsym   = {}        # Symbols defined as strings
#      self.ignore   = {}        # Ignore strings by state
#      self.errorf   = {}        # Error functions by state
#      self.eoff     = {}        # EOF functions by state
#
#      for s in self.stateinfo:
#          self.funcsym[s] = []
#      self.strsym[s] = []
#
#      if len(tsymbols) == 0:
#            self.log.error('No rules of the form t_rulename are defined')
#      self.error = True
#      return
#
#      for f in tsymbols:
#          t = self.ldict[f]
#      states, tokname = _statetoken(f, self.stateinfo)
#      self.toknames[f] = tokname
#
#      if hasattr(t, '__call__'):
#            if tokname == 'error':
#                  for s in states:
#                  self.errorf[s] = t
#      elif tokname == 'eof':
#          for s in states:
#          self.eoff[s] = t
#      elif tokname == 'ignore':
#          line = t.__code__.co_firstlineno
#      file = t.__code__.co_filename
#      self.log.error("%s:%d: Rule '%s' must be defined as a string", file, line, t.__name__)
#      self.error = True
#      else:
#                for s in states:
#          self.funcsym[s].append((f, t))
#    elif isinstance(t, StringTypes):
#        if tokname == 'ignore':
#              for s in states:
#              self.ignore[s] = t
#    if '\\' in t:
#        self.log.warning("%s contains a literal backslash '\\'", f)
#
#    elif tokname == 'error':
#        self.log.error("Rule '%s' must be defined as a function", f)
#    self.error = True
#    else:
#              for s in states:
#        self.strsym[s].append((f, t))
#    else:
#          self.log.error('%s not defined as a function or string', f)
#    self.error = True
#
## Sort the functions by line number
#    for f in self.funcsym.values():
#        f.sort(key=lambda x: x[1].__code__.co_firstlineno)
#
## Sort the strings by regular expression length
#    for s in self.strsym.values():
#        s.sort(key=lambda x: len(x[1]), reverse=True)
#
    # Validate all of the t_rules collected
    },
    validate_rules = function() {
#        for state in self.stateinfo:
#        # Validate all rules defined by functions
#
#        for fname, f in self.funcsym[state]:
#        line = f.__code__.co_firstlineno
#    file = f.__code__.co_filename
#    module = inspect.getmodule(f)
#    self.modules.add(module)
#
#    tokname = self.toknames[fname]
#    if isinstance(f, types.MethodType):
#          reqargs = 2
#    else:
#          reqargs = 1
#    nargs = f.__code__.co_argcount
#    if nargs > reqargs:
#          self.log.error("%s:%d: Rule '%s' has too many arguments", file, line, f.__name__)
#    self.error = True
#    continue
#
#    if nargs < reqargs:
#          self.log.error("%s:%d: Rule '%s' requires an argument", file, line, f.__name__)
#    self.error = True
#    continue
#
#    if not _get_regex(f):
#          self.log.error("%s:%d: No regular expression defined for rule '%s'", file, line, f.__name__)
#    self.error = True
#    continue
#
#    try:
#        c = re.compile('(?P<%s>%s)' % (fname, _get_regex(f)), re.VERBOSE | self.reflags)
#                if c.match(''):
#                      self.log.error("%s:%d: Regular expression for rule '%s' matches empty string", file, line, f.__name__)
#    self.error = True
#    except re.error as e:
#        self.log.error("%s:%d: Invalid regular expression for rule '%s'. %s", file, line, f.__name__, e)
#    if '#' in _get_regex(f):
#        self.log.error("%s:%d. Make sure '#' in rule '%s' is escaped with '\\#'", file, line, f.__name__)
#    self.error = True
#
      ## Validate all rules defined by strings
#    for name, r in self.strsym[state]:
#        tokname = self.toknames[name]
#    if tokname == 'error':
#          self.log.error("Rule '%s' must be defined as a function", name)
#    self.error = True
#    continue
#
#    if tokname not in self.tokens and tokname.find('ignore_') < 0:
#          self.log.error("Rule '%s' defined for an unspecified token %s", name, tokname)
#    self.error = True
#    continue
#
#    try:
#        c = re.compile('(?P<%s>%s)' % (name, r), re.VERBOSE | self.reflags)
#                if (c.match('')):
#                      self.log.error("Regular expression for rule '%s' matches empty string", name)
#    self.error = True
#    except re.error as e:
#        self.log.error("Invalid regular expression for rule '%s'. %s", name, e)
#    if '#' in r:
#        self.log.error("Make sure '#' in rule '%s' is escaped with '\\#'", name)
#    self.error = True
#
#    if not self.funcsym[state] and not self.strsym[state]:
#          self.log.error("No rules defined for state '%s'", state)
#    self.error = True
#
      ## Validate the error function
#    efunc = self.errorf.get(state, None)
#    if efunc:
#          f = efunc
#    line = f.__code__.co_firstlineno
#    file = f.__code__.co_filename
#    module = inspect.getmodule(f)
#    self.modules.add(module)
#
#    if isinstance(f, types.MethodType):
#          reqargs = 2
#    else:
#          reqargs = 1
#    nargs = f.__code__.co_argcount
#    if nargs > reqargs:
#          self.log.error("%s:%d: Rule '%s' has too many arguments", file, line, f.__name__)
#    self.error = True
#
#    if nargs < reqargs:
#          self.log.error("%s:%d: Rule '%s' requires an argument", file, line, f.__name__)
#    self.error = True
#
#    for module in self.modules:
#        self.validate_module(module)
    },
    # -----------------------------------------------------------------------------
    # validate_module()
    #
    # This checks to see if there are duplicated t_rulename() functions or strings
    # in the parser input file.  This is done using a simple regular expression
    # match on each line in the source code of the given module.
    # -----------------------------------------------------------------------------
    validate_module = function(module) {
#      lines, linen = inspect.getsourcelines(module)
#
#      fre = re.compile(r'\s*def\s+(t_[a-zA-Z_0-9]*)\(')
#      sre = re.compile(r'\s*(t_[a-zA-Z_0-9]*)\s*=')
#
#      counthash = {}
#      linen += 1
#      for line in lines:
#          m = fre.match(line)
#      if not m:
#            m = sre.match(line)
#      if m:
#            name = m.group(1)
#      prev = counthash.get(name)
#      if not prev:
#            counthash[name] = linen
#      else:
#            filename = inspect.getsourcefile(module)
#      self.log.error('%s:%d: Rule %s redefined. Previously defined on line %d', filename, linen, name, prev)
#      self.error = True
#      linen += 1
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

  # Collect parser information
  linfo = LexerReflect$new(module)
  linfo$get_all()
  linfo$validate_all()

  # Dump some basic debugging information
  if(debug) {
    if(length(linfo$tokens) > 0)   dbg(sprintf('lex: tokens   = %s', paste(linfo$tokens, collapse=" ")))
    else                           dbg('lex: tokens empty')
    if(length(linfo$literals) > 0) dbg(sprintf('lex: literals = %s', paste(linfo$literals, collapse=" ")))
    else                           dbg('lex: literals empty')
    if(length(linfo$states) > 0)   dbg(sprintf('lex: states   = %s', paste(linfo$states, collapse=" ")))
    else                           dbg('lex: states empty')
  }

  # Build a dictionary of valid token names
  lexobj$lextokens <- unique(linfo$tokens)

  return(lexobj)
}
