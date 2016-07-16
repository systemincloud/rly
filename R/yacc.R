#' yacc.R

#' Print debug message.
#'
#' @param msg message to display.
#' @export
dbg = function(msg) cat(c("DEBUG> ", msg, "\n"))
wrn = function(msg) cat(c("WARN> ", msg, "\n"))
err = function(msg) cat(c("ERROR> ", msg, "\n"))

#'-----------------------------------------------------------------------------
#'                        ===  LR Parsing Engine ===
#'
#' The following classes are used for the LR parser itself.  These are not
#' used during table construction and are independent of the actual LR
#' table generation algorithm
#'-----------------------------------------------------------------------------


#' This class is used to hold non-terminal grammar symbols during parsing.
#' It normally has the following attributes set:
#'        .type       = Grammar symbol type
#'        .value      = Symbol value
#'        .lineno     = Starting line number
#'        .endlineno  = Ending line number (optional, set automatically)
#'        .lexpos     = Starting lex position
#'        .endlexpos  = Ending lex position (optional, set automatically)
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
YaccSymbol <- R6Class("YaccSymbol",
  public = list(
    type = NA
  )
)


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
#' @keywords data
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
    }
  )
)


#' == LRParser ==
#' The LR Parsing engine.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
LRParser <- R6Class("LRParser",
  public = list(
    productions = NA,
    action = NA,
    goto = NA,
    errorfunc = NA,
    errorok = NA,
    statestack = NA,
    symstack = NA,
    initialize = function(lrtab, errorf) {
      self$productions <- lrtab$lr_productions
      self$action <- lrtab$lr_action
      self$goto <- lrtab$lr_goto
      self$errorfunc <- errorf
      self$errorok <- TRUE
    },
    errok = function() {
      self$errorok <- TRUE
    },
    restart = function() {
    },
    parse = function(input, lexer, debug=FALSE) {
      if(debug) dbg("LRParser:parse:start")
      lookahead <- NA                  # Current lookahead symbol
      lookaheadstack <- c()            # Stack of lookahead symbols
      actions <- self$action           # Local reference to action table (to avoid lookup on self$)
      goto <- self$goto                # Local reference to goto table (to avoid lookup on self$)
      prod <- self$productions         # Local reference to production list (to avoid lookup on self$)
      pslice <- YaccProduction$new(NA) # Production object passed to grammar rules

      if(debug) dbg("LRParser:parse: Set up the lexer and parser objects on pslice")

      pslice$lexer <- lexer
      pslice$parser <- self

      lexer$input(input)

      if(debug) dbg("LRParser:parse: Set up the state and symbol stacks")
      statestack <- c()               # Stack of parsing states
      self$statestack <- statestack
      symstack <- c()                 # Stack of grammar symbols
      self$symstack <- symstack

      pslice$stack <- symstack        # Put in the production
      errtoken <- NA                  # Err token

      # The start state is assumed to be (0,$end)

      statestack <- c(0)
      sym <- YaccSymbol$new()
      sym$type = '$end'
      symstack <- c(sym)
      state <- 0

      while(TRUE) {
        # Get the next symbol on the input.  If a lookahead symbol
        # is already set, we just use that. Otherwise, we'll pull
        # the next token off of the lookaheadstack or from the lexer

        if(debug) dbg(sprintf("LRParser:parse: State  : %s", state))

        t <- NA

        if(is.na(lookahead)) {
          if(debug) dbg("LRParser:parse: lookahead  : NA")

          if(length(lookaheadstack) == 0) lookahead <- lexer$token() # Get the next token
          else {
            lookahead <- tail(lookaheadstack,n=1)[0]
            lookaheadstack <- head(lookaheadstack,-1)
          }

          if(is.na(lookahead)) {
            lookahead <- YaccSymbol$new()
            lookahead$type <- '$end'
          }
        }

        # Check the action table
        ltype <- lookahead$type
        t <- actions[state]$get(ltype)

        if(!is.na(t)) {
          if(t > 0) {

          }
          if(t < 0) {

          }
          if(t == 0) {
            n = tail(symstack, n=1)
            result = n$value

            if(debug) dbg(sprintf('Done   : Returning %s', result))
            if(debug) dbg('PARSE DEBUG END')

            return(result)
          }
        } else stop("yacc: internal parser error!!!")
      }
    }
  )
)


#' -----------------------------------------------------------------------------
#'                           === GRAMMAR CLASS ===
#'
#' The following class represents the contents of the specified grammar along
#' with various computed properties such as first sets, follow sets, LR items, etc.
#' This data is used for critical parts of the table generation process later.
#' -----------------------------------------------------------------------------
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
Grammar <- R6Class("Grammar",
  public = list(
    initialize = function(terminals) {
    }
  )
)


#' -----------------------------------------------------------------------------
#'                            == Class LRTable ==
#'
#' This basic class represents a basic table of LR parsing information.
#' Methods for generating the tables are not defined here.  They are defined
#' in the derived class LRGeneratedTable.
#' -----------------------------------------------------------------------------
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
LRTable <- R6Class("LRTable",
    public = list(
        initialize = function() {
        }
    )
)


#' -----------------------------------------------------------------------------
#'                             == LRGeneratedTable ==
#'
#' This class implements the LR table generation algorithm.  There are no
#' public methods except for write()
#' -----------------------------------------------------------------------------
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
LRGeneratedTable <- R6Class("LRGeneratedTable",
  public = list(
    grammar = NA,
    initialize = function(grammar) {
      self$grammar = grammar
    }
  )
)


#' -----------------------------------------------------------------------------
#' parse_grammar()
#'
#' This takes a raw grammar rule string and parses it into production data
#' -----------------------------------------------------------------------------
parse_grammar = function(doc) {
  grammar <- list()
  # Split the doc string into lines
#  pstrings <- strsplit(doc, "s/\r[\n]*/\n/gm")
  pstrings <- strsplit(doc, "s/\n/gm", perl=TRUE)
  dbg(toString(pstrings))
  dbg(length(pstrings))
  for(ps in pstrings) {
    dbg(ps)
  }
}


#lastp = None
#dline = line
#for ps in pstrings:
#    dline += 1
#p = ps.split()
#if not p:
#      continue
#try:
#    if p[0] == '|':
#          # This is a continuation of a previous rule
#          if not lastp:
#                raise SyntaxError("%s:%d: Misplaced '|'" % (file, dline))
#                        prodname = lastp
#syms = p[1:]
#else:
#      prodname = p[0]
#lastp = prodname
#syms   = p[2:]
#assign = p[1]
#if assign != ':' and assign != '::=':
#      raise SyntaxError("%s:%d: Syntax error. Expected ':'" % (file, dline))
#
#              grammar.append((file, dline, prodname, syms))
#except SyntaxError:
#    raise
#except Exception:
#    raise SyntaxError('%s:%d: Syntax error in rule %r' % (file, dline, ps.strip()))
#
#            return grammar


#' -----------------------------------------------------------------------------
#' ParserReflect()
#'
#' This class represents information extracted for building a parser including
#' start symbol, error function, tokens, precedence list, action functions,
#' etc.
#' -----------------------------------------------------------------------------
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
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
    error      = NA,
    initialize = function(module, instance) {
      self$module <- module
      self$instance <- instance
      self$error <- FALSE
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
          err("'start' must be a string")
    },
    # Look for error handler
    get_error_func = function() {
      self$error_func = self$instance$p_error
    },
    # Validate the error function
    validate_error_func = function() {
      if(!is.null(self$error_func)) {
        if(typeof(self$error_func) != 'closure') {
          err("'p_error' defined, but is not a function or method")
          self$error <- True
          return
        }
        if(length(formals(self$error_func)) != 1) {
          err('p_error() requires 1 argument')
          self$error <- True
          return
        }
      }
    },
    # Get the tokens map
    get_tokens = function() {
      tokens <- self$instance$tokens

      if(is.null(tokens)) {
        err('No token list is defined')
        self$error = TRUE
        return
      }
      if(!is.vector(tokens)) {
        err('tokens must be a vector')
        self$error <- TRUE
        return
      }
      if(length(tokens) == 0) {
        err('tokens is empty')
        self$error <- TRUE
        return
      }

      self$tokens <- tokens
    },
    # Validate the tokens
    validate_tokens = function() {
      # Validate the tokens.
      if('error' %in% self$tokens) {
        err("Illegal token name 'error'. Is a reserved word")
        self$error = TRUE
        return
      }

      terminals <- list()
      for(n in self$tokens) {
        if(n %in% terminals) wrn('Token %r multiply defined', n)
        terminals[length(terminals)+1] <- n
      }
    },
    # Get the precedence map (if any)
    get_precedence = function() {
      self$prec = self$instance$precedence
    },
    # Validate and parse the precedence map
    validate_precedence = function() {
      preclist <- list()
      if(is.null(self$prec)) {
        if(!is.vector(self$prec)) {
          err("precedence must be a list")
          self$error = TRUE
          return
        }
        for(p in self$prec) {
          if(!is.vector(p)) {
            err("Bad precedence table")
            self$error = TRUE
            return
          }
          if(length(p) < 2) {
            err("Malformed precedence entry. Must be (assoc, term, ..., term)")
            self$error = TRUE
            return
          }
          assoc <- p[0]
          if(typeof(assoc) != 'character') {
            err("precedence associativity must be a string")
            self$error = TRUE
            return
          }

          level <- 0
          for(term in tail(p, -1)) {
            if(typeof(term) != 'character') {
              err("precedence items must be strings")
              self$error = TRUE
              return
            }
            preclist[[length(preclist)+1]] <- list(term, assoc, level+1)
            level <- level + 1
          }
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
        err("no rules of the form p_rulename are defined")
        self$error = TRUE
        return
      }
      for(name in self$pfuncs) {
        f <-self$instance[[name]]
        reqargs <- 2
        nargs <- length(formals(f))
        if(nargs > reqargs) {
          err(sprintf("Rule '%s' has too many arguments", name))
          self$error <- TRUE
          return
        } else if(nargs < reqargs) {
          err(sprintf("Rule '%s' requires an argument", name))
          self$error <- TRUE
          return
        } else {
#            try:
          doc <- formals(f)[['doc']]
          parsed_g <- parse_grammar(doc)
#      for g in parsed_g:
#          grammar.append((name, g))
#    except SyntaxError as e:
#        self.log.error(str(e))
#    self.error = True
        }

      }
      self$grammar <- grammar
    }
  )
)


#' Build a parser
#' @export
yacc = function(module=NA,
                args=list(),
                method='LALR',
                debug=FALSE,
                start=NA,
                check_recursion=TRUE) {

  instance <- do.call("new", args, envir=module)

  # Set start symbol if it's specified directly using an argument

  # Collect parser information from the dictionary
  pinfo <- ParserReflect$new(module, instance)
  pinfo$get_all()
  pinfo$validate_all()

#  if(pinfo$error) stop('Unable to build parser')




#  # Create a grammar object
#  grammar <- Grammar$new(pinfo$tokens)
#
#  lr <- LRGeneratedTable$new(grammar)
#
#  # Build the parser
#  parser = LRParser$new(lr, pinfo$error_func)

#  return(parser)
}