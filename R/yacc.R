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
      self$validate_modules()
      return(self$error)
    },
    # -----------------------------------------------------------------------------
    # validate_modules()
    #
    # This method checks to see if there are duplicated p_rulename() functions
    # in the parser module file.  Without this function, it is really easy for
    # users to make mistakes by cutting and pasting code fragments (and it's a real
    # bugger to try and figure out why the resulting parser doesn't work).  Therefore,
    # we just do a little regular expression pattern matching of def statements
    # to try and detect duplicates.
    # -----------------------------------------------------------------------------
    validate_modules = function() {

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

    },
    # Validate the error function
    validate_error_func = function(self) {

    },
    # Get the tokens map
    get_tokens = function() {

    },
    # Validate the tokens
    validate_tokens = function() {

    },
    # Get the precedence map (if any)
    get_precedence = function() {

    },
    # Validate and parse the precedence map
    validate_precedence = function() {

    },
    # Get all p_functions from the grammar
    get_pfunctions = function() {

    },
    # Validate all of the p_functions
    validate_pfunctions = function() {

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