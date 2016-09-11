#' yacc.R

#' Print debug message.
#'
#' @param msg message to display.
#' @export
dbg = function(msg) cat (c("DEBUG> ", msg, "\n"))
wrn = function(msg) cat (c("WARN> ", msg, "\n"))
err = function(msg) stop(c("ERROR> ", msg, "\n"))

'%nin%' <- Negate('%in%')

id = function(x) substring(capture.output(.Internal(inspect(x, 5)))[1],2,8)

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


#' -----------------------------------------------------------------------------
#'                               == LRParser ==
#'
#' The LR Parsing engine.
#' -----------------------------------------------------------------------------
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
LRParser <- R6Class("LRParser",
  public = list(
#    productions = NA,
#    action = NA,
#    goto = NA,
#    errorfunc = NA,
#    errorok = NA,
#    statestack = NA,
#    symstack = NA,
    initialize = function(lrtab, errorf) {
#      self$productions <- lrtab$lr_productions
#      self$action <- lrtab$lr_action
#      self$goto <- lrtab$lr_goto
#      self$errorfunc <- errorf
#      self$errorok <- TRUE
    }
#    errok = function() {
#      self$errorok <- TRUE
#    },
#    restart = function() {
#    },
#    parse = function(input, lexer, debug=FALSE) {
#      if(debug) dbg("LRParser:parse:start")
#      lookahead <- NA                  # Current lookahead symbol
#      lookaheadstack <- c()            # Stack of lookahead symbols
#      actions <- self$action           # Local reference to action table (to avoid lookup on self$)
#      goto <- self$goto                # Local reference to goto table (to avoid lookup on self$)
#      prod <- self$productions         # Local reference to production list (to avoid lookup on self$)
#      pslice <- YaccProduction$new(NA) # Production object passed to grammar rules
#
#      if(debug) dbg("LRParser:parse: Set up the lexer and parser objects on pslice")
#
#      pslice$lexer <- lexer
#      pslice$parser <- self
#
#      lexer$input(input)
#
#      if(debug) dbg("LRParser:parse: Set up the state and symbol stacks")
#      statestack <- c()               # Stack of parsing states
#      self$statestack <- statestack
#      symstack <- c()                 # Stack of grammar symbols
#      self$symstack <- symstack
#
#      pslice$stack <- symstack        # Put in the production
#      errtoken <- NA                  # Err token
#
#      # The start state is assumed to be (0,$end)
#
#      statestack <- c(0)
#      sym <- YaccSymbol$new()
#      sym$type = '$end'
#      symstack <- c(sym)
#      state <- 0
#
#      while(TRUE) {
#        # Get the next symbol on the input.  If a lookahead symbol
#        # is already set, we just use that. Otherwise, we'll pull
#        # the next token off of the lookaheadstack or from the lexer
#
#        if(debug) dbg(sprintf("LRParser:parse: State  : %s", state))
#
#        t <- NA
#
#        if(is.na(lookahead)) {
#          if(debug) dbg("LRParser:parse: lookahead  : NA")
#
#          if(length(lookaheadstack) == 0) lookahead <- lexer$token() # Get the next token
#          else {
#            lookahead <- tail(lookaheadstack,n=1)[0]
#            lookaheadstack <- head(lookaheadstack,-1)
#          }
#
#          if(is.na(lookahead)) {
#            lookahead <- YaccSymbol$new()
#            lookahead$type <- '$end'
#          }
#        }
#
#        # Check the action table
#        ltype <- lookahead$type
#        t <- actions[state]$get(ltype)
#
#        if(!is.na(t)) {
#          if(t > 0) {
#
#          }
#          if(t < 0) {
#
#          }
#          if(t == 0) {
#            n = tail(symstack, n=1)
#            result = n$value
#
#            if(debug) dbg(sprintf('Done   : Returning %s', result))
#            if(debug) dbg('PARSE DEBUG END')
#
#            return(result)
#          }
#        } else stop("yacc: internal parser error!!!")
#      }
#    }
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
#' class Production:
#'
#' This class stores the raw information about a single production or grammar rule.
#' A grammar rule refers to a specification such as this:
#'
#'       expr : expr PLUS term
#'
#' Here are the basic attributes defined on all productions
#'
#'       name     - Name of the production.  For example 'expr'
#'       prod     - A list of symbols on the right side ['expr','PLUS','term']
#'       prec     - Production precedence level
#'       number   - Production number.
#'       func     - Function that executes on reduce
#'
#' The following attributes are defined or optional.
#'
#'       len       - Length of the production (number of symbols on right hand side)
#'       usyms     - Set of unique symbols found in the production
#' -----------------------------------------------------------------------------
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
Production <- R6Class("Production",
  public = list(
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
    }
  )
)


#' -----------------------------------------------------------------------------
#' class LRItem
#'
#' This class represents a specific stage of parsing a production rule.  For
#' example:
#'
#'       expr : expr . PLUS term
#'
#' In the above, the "." represents the current location of the parse.  Here
#' basic attributes:
#'
#'       name       - Name of the production.  For example 'expr'
#'       prod       - A list of symbols on the right side ['expr','.', 'PLUS','term']
#'       number     - Production number.
#'
#'       lr_next      Next LR item. Example, if we are ' expr -> expr . PLUS term'
#'                    then lr_next refers to 'expr -> expr PLUS . term'
#'       lr_index   - LR item index (location of the ".") in the prod list.
#'       lookaheads - LALR lookahead symbols for this item
#'       len        - Length of the production (number of symbols on right hand side)
#'       lr_after    - List of all productions that immediately follow
#'       lr_before   - Grammar symbol immediately before
#' -----------------------------------------------------------------------------
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
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
      if(length(self$Productions) > 1)                err('Must call set_precedence() before add_production()')
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
        err(sprintf('%s: Duplicate rule %s. ', func, m))
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
      if(!(start %in% names(self$Nonterminals))) err(sprintf('start symbol %s undefined', start))
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
    # Bind all production function names to callable objects in pdict
    bind_callables = function(instance) {
      
    }
  )
)


#' -----------------------------------------------------------------------------
#'                           === LR Generator ===
#'
#' The following classes and functions are used to generate LR parsing tables on
#' a grammar.
#' -----------------------------------------------------------------------------


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
  inherit = LRTable,
  public = list(
    grammar        = NA,
    lr_method      = NA,
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
    initialize = function(grammar, method='LALR') {
      if(method %nin% c('SLR', 'LALR')) err(sprintf('Unsupported method %s', method))
      
      self$grammar   <- grammar
      self$lr_method <- method
      
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
      J <- I
      didadd <- TRUE
      while(didadd) {
        didadd <- FALSE
        for(j in J) {
          for(x in j$lr_after) {
            if     ( is.na(x$lr0_added) && private$add_count == 0)           next
            else if(!is.na(x$lr0_added) && x$lr0_added == private$add_count) next
            # Add B --> .G to J
            J <- append(J, x$lr_next)
            x$lr0_added <- private$add_count
            didadd <- TRUE
          }
        }
      }

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
      dbg('dr_relation')
      dr_set <- new.env(hash=TRUE)
      state <- as.numeric(trans[1])
      N <- trans[2]
      terms <- c()
      
      dbg(toString(C[[state]]))
      for(c in C[[state]]) {
        dbg(c$toString())
      }
      dbg(N)
      g <- self$lr0_goto(C[[state]], N)
      for(p in g) {
        dbg(p$toString())
        if(p$lr_index < p$len) {
          a <- p$prod[[p$lr_index+1]]
          dbg(toString(a))
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
      dbg('reads_relation')
      # Look for empty transitions
      rel <- list()
      state <- as.numeric(trans[1])
      N <- trans[2]
      
      g <- self$lr0_goto(C[[state]], N)
      dbg('gggggggggggggggggggggggggggggggggggggg')
      dbg(toString(g))
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
    digraph = function(X, C, nullable) {
      dbg('digraph')
      N <- new.env(hash=TRUE)
      for(x in X) N[[paste(x, collapse = ' ')]] <- 0
      dbg(toString(names(N)))
      stack <- c()
      F <- new.env(hash=TRUE)
      for(x in X) {
        dbg(toString(x))
        if(N[[paste(x, collapse = ' ')]] == 0) {
          stack <- self$traverse(x, N, stack, F, C, nullable)
          dbg('stack')
          dbg(toString(stack))
          dbg('N:START')
          for(n in names(N)) {
            dbg(toString(n))
            dbg(toString(N[[n]]))
          }
          dbg('N:STOP')
          dbg('F:START')
          for(f in names(F)) {
            dbg(toString(f))
            dbg(toString(F[[f]]))
          }
          dbg('F:STOP')
        }
      }
      return(F)
    },
    traverse = function(x, N, stack, F, C, nullable) {
      stack[[length(stack)+1]] <- x
      d <- length(stack)
      N[[paste(x, collapse = ' ')]] <- d
      F[[paste(x, collapse = ' ')]] <- self$dr_relation(C, x, nullable) # F(X) <- F'(x)
      dbg(toString(F[[paste(x, collapse = ' ')]]))
  
      rel <- self$reads_relation(C, x, nullable)                            # Get y's related to x
      dbg('rel')
      dbg(toString(rel))
      for(y in rel) {
        dbg('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
#    if(N[[y]] == 0) stack <- self$traverse(y, N, stack, F, R, FP)
#    N[[x]] <- min(N[x], N[y])
#    for(a in F[[y]])
#      if(a %in% F[[x]]) F[[x]] <- append(F[[x]], a)
    }

    if(N[[paste(x, collapse = ' ')]] == d) {
      dbg('if')
      N[[paste(tail(stack, 1)[[1]], collapse = ' ')]] <- .Machine$integer.max
      F[[paste(tail(stack, 1)[[1]], collapse = ' ')]] <- F[[paste(x, collapse = ' ')]]
      element <- tail(stack, 1)
      stack <- head(stack, -1)
#    while(element != x) {
#      N[[tail(stack, 1)]] <- .Machine$integer.max
#      F[[tail(stack, 1)]] <- F[[x]]
#      element <- tail(stack, 1)
#      stack <- head(stack, -1)
#    }
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
      dbg('compute_read_sets')
      F <- self$digraph(ntrans, C, nullable)
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
      dbg(toString(names(readsets)))
      dbg('ENDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD')
      
      # Compute lookback/includes relations
      lookd_included <- self$compute_lookback_includes(C, trans, nullable)
      lookd <- lookd_included[[1]]
      included <- lookd_included[[2]]

      # Compute LALR FOLLOW sets
      
# 		# Add all of the lookaheads
  
      # ...
  
    },
    # -----------------------------------------------------------------------------
    # lr_parse_table()
    #
    # This function constructs the parse tables for SLR or LALR
    # -----------------------------------------------------------------------------
    lr_parse_table = function() {
      Productions <- self$grammar$Productions
      Precedence  <- self$grammar$Precedence
      goto   <- self$lr_goto         # Goto array
      action <- self$lr_action       # Action array
      log    <- self$log             # Logger for output
      
      actionp <- new.env(hash=TRUE)  # Action production array (temporary)
            
      # Step 1: Construct C = { I0, I1, ... IN}, collection of LR(0) items
      # This determines the number of states
  
      C <- self$lr0_items()
      
      if(self$lr_method == 'LALR') self$add_lalr_lookaheads(C)
      
      # Build the parser table, state by state
      st <- 0
      for(I in C) {
        
      }
    }
  ),
  private = list(
    add_count = NA
  )
)

#' -----------------------------------------------------------------------------
#'                            === INTROSPECTION ===
#'
#' The following functions and classes are used to implement the PLY
#' introspection features followed by the yacc() function itself.
#' -----------------------------------------------------------------------------

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
    funcname <- gram[[1]]
    prodname <- gram[[2]]
    syms     <- gram[[3]]
    grammar$add_production(prodname, syms, funcname)
  }
  
  # Set the grammar start symbols
  if(is.na(start)) {
    if(is.null(pinfo$start)) grammar$set_start()
    else                     grammar$set_start(pinfo$start)
  } else grammar$set_start(start)
  
  # Verify the grammar structure
  undefined_symbols <- grammar$undefined_symbols()
  for(sym_prod in undefined_symbols) {
    err(sprintf('Symbol %s used, but not defined as a token or a rule', sym_prod[[1]]))
  }
  
  unused_terminals <- grammar$unused_terminals()
  for(term in unused_terminals) {
    wrn(sprintf('Token %s defined, but not used', term))
  }

  # Print out all productions to the debug log
  if(debug) {
    dbg('')
    dbg('Grammar')
    dbg('')
    n <- 0
    for(p in grammar$Productions) {
      dbg(sprintf('Rule %-5d %s', n, p$toString()))
      n <- n + 1
    }
  }

  # Find unused non-terminals
  unused_rules <- grammar$unused_rules()
  for(prod in unused_rules) wrn(sprintf("Rule %s defined, but not used", prod$name))
  
  if(length(unused_terminals) == 1) wrn('There is 1 unused token')
  if(length(unused_terminals) > 1)  wrn(sprintf('There are %d unused tokens', length(unused_terminals)))
  if(length(unused_rules) == 1)     wrn('There is 1 unused rule')
  if(length(unused_rules) > 1)      wrn(sprintf('There are %d unused rules', length(unused_rules)))

  if(debug) {
    dbg('')
	  dbg('Terminals, with rules where they appear')
	  dbg('')
	  terms <- names(grammar$Terminals)
	  terms <- sort(terms)
	  for(term in terms)
		  dbg(sprintf('%-20s : %s', term, paste(grammar$Terminals[[term]], sep=' ', collapse=' ')))
	  
	  dbg('')
	  dbg('Nonterminals, with rules where they appear')
	  dbg('')
	  nonterms <- names(grammar$Nonterminals)
	  nonterms <- sort(nonterms)
	  for(nonterm in nonterms)
		  dbg(sprintf('%-20s : %s', nonterm, paste(grammar$Nonterminals[[nonterm]], sep=' ', collapse=' ')))
	  dbg('')
  }
  
  if(check_recursion) {
    unreachable <- grammar$find_unreachable()
    for(u in unreachable) wrn(sprintf('Symbol %s is unreachable', u))
    
    infinite <- grammar$infinite_cycles()
    for(inf in infinite) err(sprintf('Infinite recursion detected for symbol %s', inf))
  }
  
  unused_prec <- grammar$unused_precedence()
  for(term_assoc in unused_prec) {
    term  <- term_assoc[[1]]
    assoc <- term_assoc[[2]]
    err(sprintf('Precedence rule %s defined for unknown symbol %s', assoc, term))
  }
  
  # Run the LRGeneratedTable on the grammar
  if(debug) dbg(sprintf('Generating %s tables', method))

  lr <- LRGeneratedTable$new(grammar, method)

  if(debug) {
    num_sr <- length(lr$sr_conflicts)
    
    # Report shift/reduce and reduce/reduce conflicts
    if     (num_sr == 1) wrn('1 shift/reduce conflict')
    else if(num_sr > 1)  wrn(sprintf('%d shift/reduce conflicts', num_sr))
    
    num_rr <- length(lr$rr_conflicts)
    if     (num_rr == 1) wrn('1 reduce/reduce conflict')
    else if(num_rr > 1)  wrn(sprintf('%d reduce/reduce conflicts', num_rr))
  }

  # Write out conflicts to the output file
  if(debug && (length(lr$sr_conflicts) > 0 || length(lr$rr_conflicts)) > 0) {
    wrn('')
    wrn('Conflicts:')
    wrn('')
    
    # ...
  }

  # ...

  #  # Build the parser
  lr$bind_callables(instance)
  parser <- LRParser$new(lr, pinfo$error_func)

  return(parser)
}