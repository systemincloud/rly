#' lex.R

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
}

#formals(g)$y




## Collect parser information from the dictionary
#linfo = LexerReflect(ldict, log=errorlog, reflags=reflags)
#linfo.get_all()
#if not optimize:
#      if linfo.validate_all():
#            raise SyntaxError("Can't build lexer")
#
#if optimize and lextab:
#      try:
#      lexobj.readtab(lextab, ldict)
#token = lexobj.token
#input = lexobj.input
#lexer = lexobj
#return lexobj
#
#except ImportError:
#    pass
#
## Dump some basic debugging information
#if debug:
#      debuglog.info('lex: tokens   = %r', linfo.tokens)
#debuglog.info('lex: literals = %r', linfo.literals)
#debuglog.info('lex: states   = %r', linfo.stateinfo)
#
## Build a dictionary of valid token names
#lexobj.lextokens = set()
#for n in linfo.tokens:
#    lexobj.lextokens.add(n)
#
## Get literals specification
#if isinstance(linfo.literals, (list, tuple)):
#    lexobj.lexliterals = type(linfo.literals[0])().join(linfo.literals)
#else:
#      lexobj.lexliterals = linfo.literals
#
#lexobj.lextokens_all = lexobj.lextokens | set(lexobj.lexliterals)
#
## Get the stateinfo dictionary
#stateinfo = linfo.stateinfo
#
#regexs = {}
## Build the master regular expressions
#for state in stateinfo:
#    regex_list = []
#
## Add rules defined by functions first
#for fname, f in linfo.funcsym[state]:
#    line = f.__code__.co_firstlineno
#file = f.__code__.co_filename
#regex_list.append('(?P<%s>%s)' % (fname, _get_regex(f)))
#        if debug:
#              debuglog.info("lex: Adding rule %s -> '%s' (state '%s')", fname, _get_regex(f), state)
#
## Now add all of the simple rules
#for name, r in linfo.strsym[state]:
#    regex_list.append('(?P<%s>%s)' % (name, r))
#            if debug:
#                  debuglog.info("lex: Adding rule %s -> '%s' (state '%s')", name, r, state)
#
#regexs[state] = regex_list
#
## Build the master regular expressions
#
#if debug:
#      debuglog.info('lex: ==== MASTER REGEXS FOLLOW ====')
#
#for state in regexs:
#    lexre, re_text, re_names = _form_master_re(regexs[state], reflags, ldict, linfo.toknames)
#lexobj.lexstatere[state] = lexre
#lexobj.lexstateretext[state] = re_text
#lexobj.lexstaterenames[state] = re_names
#if debug:
#      for i, text in enumerate(re_text):
#    debuglog.info("lex: state '%s' : regex[%d] = '%s'", state, i, text)
#
## For inclusive states, we need to add the regular expressions from the INITIAL state
#for state, stype in stateinfo.items():
#    if state != 'INITIAL' and stype == 'inclusive':
#          lexobj.lexstatere[state].extend(lexobj.lexstatere['INITIAL'])
#lexobj.lexstateretext[state].extend(lexobj.lexstateretext['INITIAL'])
#lexobj.lexstaterenames[state].extend(lexobj.lexstaterenames['INITIAL'])
#
#lexobj.lexstateinfo = stateinfo
#lexobj.lexre = lexobj.lexstatere['INITIAL']
#lexobj.lexretext = lexobj.lexstateretext['INITIAL']
#lexobj.lexreflags = reflags
#
## Set up ignore variables
#lexobj.lexstateignore = linfo.ignore
#lexobj.lexignore = lexobj.lexstateignore.get('INITIAL', '')
#
## Set up error functions
#lexobj.lexstateerrorf = linfo.errorf
#lexobj.lexerrorf = linfo.errorf.get('INITIAL', None)
#if not lexobj.lexerrorf:
#      errorlog.warning('No t_error rule is defined')
#
## Set up eof functions
#lexobj.lexstateeoff = linfo.eoff
#lexobj.lexeoff = linfo.eoff.get('INITIAL', None)
#
## Check state information for ignore and error rules
#for s, stype in stateinfo.items():
#    if stype == 'exclusive':
#          if s not in linfo.errorf:
#                errorlog.warning("No error rule is defined for exclusive state '%s'", s)
#if s not in linfo.ignore and lexobj.lexignore:
#      errorlog.warning("No ignore rule is defined for exclusive state '%s'", s)
#elif stype == 'inclusive':
#    if s not in linfo.errorf:
#          linfo.errorf[s] = linfo.errorf.get('INITIAL', None)
#if s not in linfo.ignore:
#      linfo.ignore[s] = linfo.ignore.get('INITIAL', '')
#
## Create global versions of the token() and input() functions
#token = lexobj.token
#input = lexobj.input
#lexer = lexobj
#
## If in optimize mode, we write the lextab
#if lextab and optimize:
#      if outputdir is None:
#            # If no output directory is set, the location of the output files
#            # is determined according to the following rules:
#            #     - If lextab specifies a package, files go into that package directory
#            #     - Otherwise, files go in the same directory as the specifying module
#            if isinstance(lextab, types.ModuleType):
#                  srcfile = lextab.__file__
#else:
#          if '.' not in lextab:
#    srcfile = ldict['__file__']
#else:
#      parts = lextab.split('.')
#pkgname = '.'.join(parts[:-1])
#exec('import %s' % pkgname)
#        srcfile = getattr(sys.modules[pkgname], '__file__', '')
#outputdir = os.path.dirname(srcfile)
#try:
#    lexobj.writetab(lextab, outputdir)
#except IOError as e:
#    errorlog.warning("Couldn't write lextab module %r. %s" % (lextab, e))
#
#            return lexobj