#' yacc.R

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

#        def __init__(self, terminals):
#    self.Productions  = [None]  # A list of all of the productions.  The first
## entry is always reserved for the purpose of
## building an augmented grammar
#
#self.Prodnames    = {}      # A dictionary mapping the names of nonterminals to a list of all
## productions of that nonterminal.
#
#self.Prodmap      = {}      # A dictionary that is only used to detect duplicate
## productions.
#
#self.Terminals    = {}      # A dictionary mapping the names of terminal symbols to a
## list of the rules where they are used.
#
#for term in terminals:
#    self.Terminals[term] = []
#
#self.Terminals['error'] = []
#
#self.Nonterminals = {}      # A dictionary mapping names of nonterminals to a list
## of rule numbers where they are used.
#
#self.First        = {}      # A dictionary of precomputed FIRST(x) symbols
#
#self.Follow       = {}      # A dictionary of precomputed FOLLOW(x) symbols
#
#self.Precedence   = {}      # Precedence rules for each terminal. Contains tuples of the
## form ('right',level) or ('nonassoc', level) or ('left',level)
#
#self.UsedPrecedence = set() # Precedence rules that were actually used by the grammer.
## This is only used to provide error checking and to generate
## a warning about unused precedence rules.
#
#self.Start = None           # Starting symbol for the grammar
#
#
#def __len__(self):
#    return len(self.Productions)
#
#def __getitem__(self, index):
#    return self.Productions[index]
#
## -----------------------------------------------------------------------------
## set_precedence()
##
## Sets the precedence for a given terminal. assoc is the associativity such as
## 'left','right', or 'nonassoc'.  level is a numeric level.
##
## -----------------------------------------------------------------------------
#
#def set_precedence(self, term, assoc, level):
#    assert self.Productions == [None], 'Must call set_precedence() before add_production()'
#if term in self.Precedence:
#      raise GrammarError('Precedence already specified for terminal %r' % term)
#              if assoc not in ['left', 'right', 'nonassoc']:
#                    raise GrammarError("Associativity must be one of 'left','right', or 'nonassoc'")
#self.Precedence[term] = (assoc, level)
#
## -----------------------------------------------------------------------------
## add_production()
##
## Given an action function, this function assembles a production rule and
## computes its precedence level.
##
## The production rule is supplied as a list of symbols.   For example,
## a rule such as 'expr : expr PLUS term' has a production name of 'expr' and
## symbols ['expr','PLUS','term'].
##
## Precedence is determined by the precedence of the right-most non-terminal
## or the precedence of a terminal specified by %prec.
##
## A variety of error checks are performed to make sure production symbols
## are valid and that %prec is used correctly.
## -----------------------------------------------------------------------------
#
#def add_production(self, prodname, syms, func=None, file='', line=0):
#
#    if prodname in self.Terminals:
#          raise GrammarError('%s:%d: Illegal rule name %r. Already defined as a token' % (file, line, prodname))
#                  if prodname == 'error':
#                        raise GrammarError('%s:%d: Illegal rule name %r. error is a reserved word' % (file, line, prodname))
#                                if not _is_identifier.match(prodname):
#                                      raise GrammarError('%s:%d: Illegal rule name %r' % (file, line, prodname))
#
#                                              # Look for literal tokens
#                                              for n, s in enumerate(syms):
#                                              if s[0] in "'\"":
#                                                    try:
#                                                    c = eval(s)
#if (len(c) > 1):
#      raise GrammarError('%s:%d: Literal token %s in rule %r may only be a single character' %
#              (file, line, s, prodname))
#if c not in self.Terminals:
#      self.Terminals[c] = []
#syms[n] = c
#continue
#except SyntaxError:
#    pass
#if not _is_identifier.match(s) and s != '%prec':
#      raise GrammarError('%s:%d: Illegal name %r in rule %r' % (file, line, s, prodname))
#
#              # Determine the precedence level
#              if '%prec' in syms:
#              if syms[-1] == '%prec':
#                    raise GrammarError('%s:%d: Syntax error. Nothing follows %%prec' % (file, line))
#                            if syms[-2] != '%prec':
#                                  raise GrammarError('%s:%d: Syntax error. %%prec can only appear at the end of a grammar rule' %
#                                          (file, line))
#precname = syms[-1]
#prodprec = self.Precedence.get(precname)
#if not prodprec:
#      raise GrammarError('%s:%d: Nothing known about the precedence of %r' % (file, line, precname))
#              else:
#                    self.UsedPrecedence.add(precname)
#del syms[-2:]     # Drop %prec from the rule
#else:
#      # If no %prec, precedence is determined by the rightmost terminal symbol
#      precname = rightmost_terminal(syms, self.Terminals)
#prodprec = self.Precedence.get(precname, ('right', 0))
#
## See if the rule is already in the rulemap
#map = '%s -> %s' % (prodname, syms)
#    if map in self.Prodmap:
#          m = self.Prodmap[map]
#raise GrammarError('%s:%d: Duplicate rule %s. ' % (file, line, m) +
#        'Previous definition at %s:%d' % (m.file, m.line))
#
#        # From this point on, everything is valid.  Create a new Production instance
#        pnumber  = len(self.Productions)
#if prodname not in self.Nonterminals:
#      self.Nonterminals[prodname] = []
#
## Add the production number to Terminals and Nonterminals
#for t in syms:
#    if t in self.Terminals:
#          self.Terminals[t].append(pnumber)
#else:
#      if t not in self.Nonterminals:
#            self.Nonterminals[t] = []
#self.Nonterminals[t].append(pnumber)
#
## Create a production and add it to the list of productions
#p = Production(pnumber, prodname, syms, prodprec, func, file, line)
#self.Productions.append(p)
#self.Prodmap[map] = p
#
## Add to the global productions list
#try:
#    self.Prodnames[prodname].append(p)
#except KeyError:
#    self.Prodnames[prodname] = [p]
#
## -----------------------------------------------------------------------------
## set_start()
##
## Sets the starting symbol and creates the augmented grammar.  Production
## rule 0 is S' -> start where start is the start symbol.
## -----------------------------------------------------------------------------
#
#def set_start(self, start=None):
#    if not start:
#          start = self.Productions[1].name
#if start not in self.Nonterminals:
#      raise GrammarError('start symbol %s undefined' % start)
#              self.Productions[0] = Production(0, "S'", [start])
#self.Nonterminals[start].append(0)
#self.Start = start
#
## -----------------------------------------------------------------------------
## find_unreachable()
##
## Find all of the nonterminal symbols that can't be reached from the starting
## symbol.  Returns a list of nonterminals that can't be reached.
## -----------------------------------------------------------------------------
#
#def find_unreachable(self):
#
#    # Mark all symbols that are reachable from a symbol s
#    def mark_reachable_from(s):
#    if s in reachable:
#          return
#reachable.add(s)
#for p in self.Prodnames.get(s, []):
#    for r in p.prod:
#    mark_reachable_from(r)
#
#reachable = set()
#mark_reachable_from(self.Productions[0].prod[0])
#return [s for s in self.Nonterminals if s not in reachable]
#
## -----------------------------------------------------------------------------
## infinite_cycles()
##
## This function looks at the various parsing rules and tries to detect
## infinite recursion cycles (grammar rules where there is no possible way
## to derive a string of only terminals).
## -----------------------------------------------------------------------------
#
#def infinite_cycles(self):
#    terminates = {}
#
## Terminals:
#for t in self.Terminals:
#    terminates[t] = True
#
#terminates['$end'] = True
#
## Nonterminals:
#
## Initialize to false:
#for n in self.Nonterminals:
#    terminates[n] = False
#
## Then propagate termination until no change:
#while True:
#      some_change = False
#for (n, pl) in self.Prodnames.items():
#    # Nonterminal n terminates iff any of its productions terminates.
#    for p in pl:
#    # Production p terminates iff all of its rhs symbols terminate.
#    for s in p.prod:
#    if not terminates[s]:
#          # The symbol s does not terminate,
#          # so production p does not terminate.
#          p_terminates = False
#break
#else:
#      # didn't break from the loop,
#      # so every symbol s terminates
#      # so production p terminates.
#      p_terminates = True
#
#if p_terminates:
#      # symbol n terminates!
#      if not terminates[n]:
#            terminates[n] = True
#some_change = True
## Don't need to consider any more productions for this n.
#break
#
#if not some_change:
#      break
#
#infinite = []
#for (s, term) in terminates.items():
#    if not term:
#          if s not in self.Prodnames and s not in self.Terminals and s != 'error':
#                # s is used-but-not-defined, and we've already warned of that,
#                # so it would be overkill to say that it's also non-terminating.
#                pass
#else:
#      infinite.append(s)
#
#return infinite
#
## -----------------------------------------------------------------------------
## undefined_symbols()
##
## Find all symbols that were used the grammar, but not defined as tokens or
## grammar rules.  Returns a list of tuples (sym, prod) where sym in the symbol
## and prod is the production where the symbol was used.
## -----------------------------------------------------------------------------
#def undefined_symbols(self):
#    result = []
#for p in self.Productions:
#    if not p:
#          continue
#
#for s in p.prod:
#    if s not in self.Prodnames and s not in self.Terminals and s != 'error':
#          result.append((s, p))
#return result
#
## -----------------------------------------------------------------------------
## unused_terminals()
##
## Find all terminals that were defined, but not used by the grammar.  Returns
## a list of all symbols.
## -----------------------------------------------------------------------------
#def unused_terminals(self):
#    unused_tok = []
#for s, v in self.Terminals.items():
#    if s != 'error' and not v:
#          unused_tok.append(s)
#
#return unused_tok
#
## ------------------------------------------------------------------------------
## unused_rules()
##
## Find all grammar rules that were defined,  but not used (maybe not reachable)
## Returns a list of productions.
## ------------------------------------------------------------------------------
#
#def unused_rules(self):
#    unused_prod = []
#for s, v in self.Nonterminals.items():
#    if not v:
#          p = self.Prodnames[s][0]
#unused_prod.append(p)
#return unused_prod
#
## -----------------------------------------------------------------------------
## unused_precedence()
##
## Returns a list of tuples (term,precedence) corresponding to precedence
## rules that were never used by the grammar.  term is the name of the terminal
## on which precedence was applied and precedence is a string such as 'left' or
## 'right' corresponding to the type of precedence.
## -----------------------------------------------------------------------------
#
#def unused_precedence(self):
#    unused = []
#for termname in self.Precedence:
#    if not (termname in self.Terminals or termname in self.UsedPrecedence):
#          unused.append((termname, self.Precedence[termname][0]))
#
#return unused
#
## -------------------------------------------------------------------------
## _first()
##
## Compute the value of FIRST1(beta) where beta is a tuple of symbols.
##
## During execution of compute_first1, the result may be incomplete.
## Afterward (e.g., when called from compute_follow()), it will be complete.
## -------------------------------------------------------------------------
#def _first(self, beta):
#
#    # We are computing First(x1,x2,x3,...,xn)
#    result = []
#for x in beta:
#    x_produces_empty = False
#
## Add all the non-<empty> symbols of First[x] to the result.
#for f in self.First[x]:
#    if f == '<empty>':
#          x_produces_empty = True
#else:
#      if f not in result:
#            result.append(f)
#
#if x_produces_empty:
#      # We have to consider the next x in beta,
#      # i.e. stay in the loop.
#      pass
#else:
#      # We don't have to consider any further symbols in beta.
#      break
#else:
#      # There was no 'break' from the loop,
#      # so x_produces_empty was true for all x in beta,
#      # so beta produces empty as well.
#      result.append('<empty>')
#
#return result
#
## -------------------------------------------------------------------------
## compute_first()
##
## Compute the value of FIRST1(X) for all symbols
## -------------------------------------------------------------------------
#def compute_first(self):
#    if self.First:
#          return self.First
#
## Terminals:
#for t in self.Terminals:
#    self.First[t] = [t]
#
#self.First['$end'] = ['$end']
#
## Nonterminals:
#
## Initialize to the empty set:
#for n in self.Nonterminals:
#    self.First[n] = []
#
## Then propagate symbols until no change:
#while True:
#      some_change = False
#for n in self.Nonterminals:
#    for p in self.Prodnames[n]:
#    for f in self._first(p.prod):
#    if f not in self.First[n]:
#          self.First[n].append(f)
#some_change = True
#if not some_change:
#      break
#
#return self.First
#
## ---------------------------------------------------------------------
## compute_follow()
##
## Computes all of the follow sets for every non-terminal symbol.  The
## follow set is the set of all symbols that might follow a given
## non-terminal.  See the Dragon book, 2nd Ed. p. 189.
## ---------------------------------------------------------------------
#def compute_follow(self, start=None):
#    # If already computed, return the result
#    if self.Follow:
#          return self.Follow
#
## If first sets not computed yet, do that first.
#if not self.First:
#      self.compute_first()
#
## Add '$end' to the follow list of the start symbol
#for k in self.Nonterminals:
#    self.Follow[k] = []
#
#if not start:
#      start = self.Productions[1].name
#
#self.Follow[start] = ['$end']
#
#while True:
#      didadd = False
#for p in self.Productions[1:]:
#    # Here is the production set
#    for i, B in enumerate(p.prod):
#    if B in self.Nonterminals:
#          # Okay. We got a non-terminal in a production
#          fst = self._first(p.prod[i+1:])
#hasempty = False
#for f in fst:
#    if f != '<empty>' and f not in self.Follow[B]:
#          self.Follow[B].append(f)
#didadd = True
#if f == '<empty>':
#      hasempty = True
#if hasempty or i == (len(p.prod)-1):
#      # Add elements of follow(a) to follow(b)
#      for f in self.Follow[p.name]:
#      if f not in self.Follow[B]:
#            self.Follow[B].append(f)
#didadd = True
#if not didadd:
#      break
#return self.Follow
#
#
## -----------------------------------------------------------------------------
## build_lritems()
##
## This function walks the list of productions and builds a complete set of the
## LR items.  The LR items are stored in two ways:  First, they are uniquely
## numbered and placed in the list _lritems.  Second, a linked list of LR items
## is built for each production.  For example:
##
##   E -> E PLUS E
##
## Creates the list
##
##  [E -> . E PLUS E, E -> E . PLUS E, E -> E PLUS . E, E -> E PLUS E . ]
## -----------------------------------------------------------------------------
#
#def build_lritems(self):
#    for p in self.Productions:
#    lastlri = p
#i = 0
#lr_items = []
#while True:
#      if i > len(p):
#            lri = None
#else:
#      lri = LRItem(p, i)
## Precompute the list of productions immediately following
#try:
#    lri.lr_after = self.Prodnames[lri.prod[i+1]]
#except (IndexError, KeyError):
#    lri.lr_after = []
#try:
#    lri.lr_before = lri.prod[i-1]
#except IndexError:
#    lri.lr_before = None
#
#lastlri.lr_next = lri
#if not lri:
#      break
#lr_items.append(lri)
#lastlri = lri
#i += 1
#p.lr_items = lr_items


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
        pdict = NA,
        initialize = function(pdict) {
          self$pdict = pdict
        }
    )
)


#' Build a parser
#' @export
yacc = function(module=NA,
                debug=FALSE,
                start=NA,
                check_recursion=TRUE,
                write_tables=True,
                picklefile=NA) {
#  items = [(k, getattr(module, k)) for k in dir(module)]

  pdict <- maps <- new.env(hash=TRUE)

  # Collect parser information from the dictionary
  pinfo <- ParserReflect$new(pdict)

  # Create a grammar object
  grammar <- Grammar$new(pinfo$tokens)

  lr <- LRGeneratedTable$new(grammar)

  # Build the parser
  parser = LRParser$new(lr, pinfo$error_func)

  return(parser)
}