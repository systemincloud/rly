#' yacc.R

#' Print debug message.
#'
#' @param msg message to display.
#' @export
dbg = function(msg) cat (c("DEBUG> ", msg, "\n"))
wrn = function(msg) cat (c("WARN> ", msg, "\n"))
err = function(msg) stop(c("ERROR> ", msg, "\n"))

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
#'                          === Grammar Representation ===
#'
#' The following functions, classes, and variables are used to represent and
#' manipulate the rules that make up a grammar.
#' -----------------------------------------------------------------------------

#' regex matching identifiers
is_identifier <- '^[a-zA-Z0-9_-]+$'


#' -----------------------------------------------------------------------------
#' rightmost_terminal()
#'
#' Return the rightmost terminal from a list of symbols.  Used in add_production()
#' -----------------------------------------------------------------------------
rightmost_terminal = function(symbols, terminals) {
  i <- length(symbols) - 1
  while(i >= 1) {
    if(symbols[[i]] %in% names(terminals)) return(symbols[i])
    i <- i - 1
  }
  return(NULL)
}


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
    Productions = NA,
    Prodnames = NA,
    Prodmap = NA,
    Terminals = NA,
    Nonterminals = NA,
    First = NA,
    Follow = NA,
    Precedence = NA,
    UsedPrecedence = NA,
    Start = NA,
    initialize = function(terminals) {
      self$Productions <- list()           # - A list of all of the productions.  The first
                                           #   entry is always reserved for the purpose of
                                           #   building an augmented grammar      
      self$Prodnames <- new.env(hash=TRUE) # - A dictionary mapping the names of nonterminals to a list of all
                                           #   productions of that nonterminal.
      self$Prodmap <- new.env(hash=TRUE)   # - A dictionary that is only used to detect duplicate
                                           #   productions.
      self$Terminals <- new.env(hash=TRUE) # - A dictionary mapping the names of terminal symbols to a
                                           #   list of the rules where they are used.
      
      for(term in terminals) {
        self$Terminals[[term]] <- list()
      }
      
      self$Terminals[['error']] <- c()
      
      self$Nonterminals <- new.env(hash=TRUE) # - A dictionary mapping names of nonterminals to a list
                                              #   of rule numbers where they are used.
      self$First <- new.env(hash=TRUE)        # - A dictionary of precomputed FIRST(x) symbols
      self$Follow <- new.env(hash=TRUE)       # - A dictionary of precomputed FOLLOW(x) symbols
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
      if(length(self$Productions) != 0)                err('Must call set_precedence() before add_production()')
      if(term %in% names(self$recedence))              err(sprintf('Precedence already specified for terminal %s', term))
      if(!(assoc %in% c('left', 'right', 'nonassoc'))) err("Associativity must be one of 'left','right', or 'nonassoc'")
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
      if(prodname %in% names(self$Terminals))        err(sprintf('%s: Illegal rule name %s. Already defined as a token', func, prodname))
      if(prodname == 'error')                        err(sprintf('%s: Illegal rule name %s. error is a reserved word', func, prodname))
      if(!grepl(is_identifier, prodname, perl=TRUE)) err(sprintf('%s: Illegal rule name %s', func, prodname))
      
      # Look for literal tokens
      for(s in syms) {
        if(substr(s, 1, 1) %in% c("'", "\"")) {
          c <- eval(parse(text=s))
          if(nchar(c) > 1) err(sprintf('%s: Literal token %s in rule %s may only be a single character', func, s, prodname))
          if(!(c %in% names(self$Terminals))) self$Terminals[[c]] <- c()
          next
        }
        if(!grepl(is_identifier, s, perl=TRUE) && s != '%prec') err(sprintf('%s: Illegal name %s in rule %s', func,s, prodname))
      }
            
      # Determine the precedence level
      if('%prec' %in% syms) {
        if(syms[length(syms)]     == '%prec') err(sprintf('%s: Syntax error. Nothing follows %%prec', func))
        if(syms[length(syms) - 1] != '%prec') err(sprintf('%s: Syntax error. %%prec can only appear at the end of a grammar rule', func))
       
        precname <- syms[length(syms)]
        prodprec <- self$Precedence[[precname]]
       
        if(is.null(prodprec)) err(sprintf('%s: Nothing known about the precedence of %s', func, precname))
        else self$UsedPrecedence[[length(self$UsedPrecedence)+1]] <- precname
        # Drop %prec from the rule
        syms <- setdiff(syms, c(syms[length(syms) - 1]))
      } else {
        # If no %prec, precedence is determined by the rightmost terminal symbol
        precname <- rightmost_terminal(syms, self$Terminals)
        if(!is.null(precname)) prodprec <- self$Precedence[[precname]]
        else prodprec <- NULL
        
        if(is.null(prodprec)) prodprec <- list('right', 0)
      }
    
      # See if the rule is already in the rulemap
      map <- sprintf('%s -> %s', prodname, syms)
      if(map %in% names(self$Prodmap)) {
        m <- self$Prodmap[[map]]
        err(sprintf('%s: Duplicate rule %s. ', func, m))
      }
        
      # From this point on, everything is valid.  Create a new Production instance
      pnumber <- length(names(self$Productions))
      if(!(prodname %in% names(self$Nonterminals))) self$Nonterminals[prodname] <- c()
    
      # Add the production number to Terminals and Nonterminals
      for(t in syms[[1]]) {
#        if(t %in% names(self$Terminals)) self$Terminals[[t]][[length(self$Terminals[[t]])+1]] <- pnumber
#        else {
#          if(!(t %in% self$Nonterminals)) self$Nonterminals[[t]] <- c()
#          self$Nonterminals[[t]][[length(self$Nonterminals[[t]])+1]] <- pnumber
#        }
      }
      
      # Create a production and add it to the list of productions
#      p <- Production$new(pnumber, prodname, syms, prodprec, func)
#      self$Productions[[length(self$Productions)+1]] <-p
#      self$Prodmap[[map]] <- p
#    
#      # Add to the global productions list
#      self$Prodnames[[prodname]] <- p
    },
    # -----------------------------------------------------------------------------
    # set_start()
    #
    # Sets the starting symbol and creates the augmented grammar.  Production
    # rule 0 is S' -> start where start is the start symbol.
    # -----------------------------------------------------------------------------
    set_start = function(start=NA) {
    },
    # -----------------------------------------------------------------------------
    # find_unreachable()
    #
    # Find all of the nonterminal symbols that can't be reached from the starting
    # symbol.  Returns a list of nonterminals that can't be reached.
    # -----------------------------------------------------------------------------
    find_unreachable = function() {
    },
    # -----------------------------------------------------------------------------
    # infinite_cycles()
    #
    # This function looks at the various parsing rules and tries to detect
    # infinite recursion cycles (grammar rules where there is no possible way
    # to derive a string of only terminals).
    # -----------------------------------------------------------------------------
    infinite_cycles = function() {
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
#        if(not p) next
        for(s in p$prod) {
          if(!(s %in% self$Prodnames) && !(s %in% self$Terminals) && s != 'error')
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
    },
    # ------------------------------------------------------------------------------
    # unused_rules()
    #
    # Find all grammar rules that were defined,  but not used (maybe not reachable)
    # Returns a list of productions.
    # ------------------------------------------------------------------------------
    unused_rules = function() {
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
    },
    # -------------------------------------------------------------------------
    # compute_first()
    #
    # Compute the value of FIRST1(X) for all symbols
    # -------------------------------------------------------------------------
    compute_first = function() {
    },
    # ---------------------------------------------------------------------
    # compute_follow()
    #
    # Computes all of the follow sets for every non-terminal symbol.  The
    # follow set is the set of all symbols that might follow a given
    # non-terminal.  See the Dragon book, 2nd Ed. p. 189.
    # ---------------------------------------------------------------------
    compute_follow = function(start=None) {
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
    }
  )
)


#' -----------------------------------------------------------------------------
#' parse_grammar()
#'
#' This takes a raw grammar rule string and parses it into production data
#' -----------------------------------------------------------------------------
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
  		  if(is.na(lastp)) err(sprintf("%s: Misplaced '|'", name))
  		  prodname <- lastp
  		  syms <- tail(p, -1)
  	  } else {
        prodname <- p[1]
        lastp <- prodname
        syms <- tail(p, -2)
        assign <- p[2]
        if(assign != ':' && assign != '::=') err(sprintf("%s: Syntax error. Expected ':'", name))
      }
      grammar[[length(grammar)+1]] <- list(name, prodname, syms)
    }, error = function(e) { if(startsWith(e[[1]], "ERROR>")) stop(e[[1]]) 
                             else err(sprintf("%s: Syntax error in rule %s", name, ps))})
  
  }
  return(grammar)
}


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
    initialize = function(module, instance) {
      self$module <- module
      self$instance <- instance
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
      self$error_func <- self$instance$p_error
    },
    # Validate the error function
    validate_error_func = function() {
      if(!is.null(self$error_func)) {
        if(typeof(self$error_func) != 'closure')  err("'p_error' defined, but is not a function or method")
        if(length(formals(self$error_func)) != 1) err('p_error() requires 1 argument')
      }
    },
    # Get the tokens map
    get_tokens = function() {
      tokens <- self$instance$tokens

      if(is.null(tokens))                                     err('No token list is defined')
      if(!is.vector(tokens) || typeof(tokens) != 'character') err('tokens must be a vector of strings')
      if(length(tokens) == 0)                                 err('tokens is empty')

      self$tokens <- tokens
    },
    # Validate the tokens
    validate_tokens = function() {
      # Validate the tokens.
      if('error' %in% self$tokens) err("Illegal token name 'error'. Is a reserved word")

      terminals <- list()
      for(n in self$tokens) {
        if(n %in% terminals) wrn('Token %r multiply defined', n)
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
        if(!is.list(self$prec)) err("precedence must be a list")
        
        for(p in self$prec) {
          if(!is.vector(p) || typeof(p) != 'character') err("Bad precedence table")
          if(length(p) < 2) err("Malformed precedence entry. Must be (assoc, term, ..., term)")
          assoc <- p[[1]]
          if(typeof(assoc) != 'character') err("precedence associativity must be a string")

          level <- 0
          for(term in tail(p, -1)) {
            if(typeof(term) != 'character') err("precedence items must be strings")
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
      if(length(self$pfuncs) == 0) err("no rules of the form p_rulename are defined")
      for(name in self$pfuncs) {
        f <-self$instance[[name]]
        reqargs <- 2
        nargs <- length(formals(f))
        if(nargs > reqargs)              err(sprintf("Rule '%s' has too many arguments", name))
        if(is.null(formals(f)[['doc']])) err(sprintf("No documentation string specified in function %s'", name))
        if(nargs < reqargs)              err(sprintf("Rule '%s' requires an argument", name))

        doc <- formals(f)[['doc']]
        parsed_g <- parse_grammar(name, doc)
        for(g in parsed_g)
          grammar[[length(grammar)+1]] <- g
      }
      
      # Secondary validation step that looks for p_ definitions that are not functions
      # or functions that look like they might be grammar rules.
      for(name in names(self$instance)) {
        if(substr(name, 1, 2) == 'p_' && typeof(self$instance[[name]]) == 'closure') next
        if(substr(name, 1, 2) == 't_')                                               next
        if(substr(name, 1, 2) == 'p_' && name != 'p_error')                          wrn(sprintf('%s not defined as a function', name))

        if(typeof(self$instance[[name]]) == 'closure' && length(formals(self$instance[[name]])) == 2) {
          doc <- formals(self$instance[[name]])[['doc']]
          if(!is.null(doc)) {
            d <- strsplit(doc, " ")[[1]]
            tryCatch({
                if(d[2]  == ':') wrn(sprintf('%s: Possible grammar rule defined without p_ prefix', name))
            }, error = function(e) {})
          }
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

  # Set start symbol if it's specified directly using an argument
  if(!is.na(start)) {
    module$public_methods[['start']] <- start
  }
                
  instance <- do.call("new", args, envir=module)

  # Collect parser information from the dictionary
  pinfo <- ParserReflect$new(module, instance)
  pinfo$get_all()
  pinfo$validate_all()
  
  if(is.null(pinfo$error_func)) wrn('no p_error() function is defined')

  # Create a grammar object
  grammar <- Grammar$new(pinfo$tokens)

  # Set precedence level for terminals
  for(term_assoc_level in pinfo$preclist) {
    grammar$set_precedence(term_assoc_level[[1]], 
                           term_assoc_level[[2]], 
                           term_assoc_level[[3]])
  }
  
  # Add productions to the grammar
  for(gram in pinfo$grammar) {
    funcname <- gram[1]
    prodname <- gram[2]
    syms     <- gram[[3]]
    grammar$add_production(prodname, syms, funcname)
  }
  
  # Set the grammar start symbols
  if(is.na(start)) grammar$set_start(pinfo$start)
  else             grammar$set_start(start)
  
  # Verify the grammar structure
  undefined_symbols <- grammar$undefined_symbols()
  for(sym_prod in undefined_symbols) {
    err(sprintf('Symbol %s used, but not defined as a token or a rule', sym))
  }
  
#  lr <- LRGeneratedTable$new(grammar)
#
#  # Build the parser
#  parser = LRParser$new(lr, pinfo$error_func)

#  return(parser)
}