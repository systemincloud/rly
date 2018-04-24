### Changes in v1.6.1

#### BUG FIXES

  1. Fix: no visible global function definition for 'is'

### Changes in v1.6.0

#### NEW FEATURES

  1. Dynamic regex in function. Issue #5.

### Changes in v1.5.0

#### NEW FEATURES

  1. Implement proper error handling.

#### NOTES

  1. DESCRIPTION. R6 and futile.logger in Imports not in Depends.

### Changes in v1.4.2

#### BUG FIXES

  1. Fix: Error in log$info("  ! shift/reduce conflict ...

### Changes in v1.4.1

#### BUG FIXES

  1. Fix: Error: object 'rprec_rlevel' not found.

  2. Fix: could not find function "appender.console".

### Changes in v1.4.0

#### NEW FEATURES

  1. You can define literals as a string of characters e.g.: literals = ':;,=*{}()<>[]'.

### Changes in v1.3.0

#### NOTES

  1. Refactor demos.

  2. Declare Depends: R (>= 3.3.0) in DESCRIPTION file.

### Changes in v1.2.1

#### NOTES

  1. Single quote the software names 'lex' and 'yacc' both in Title and Description.

### Changes in v1.2.0

#### NEW FEATURES

  1. LRParser : add function restart.

#### BUG FIXES

  1. Error when no literals defined at all.

  2. YaccProduction : Function get don't accept negative arguments.

### Changes in v1.1.0

#### NEW FEATURES

  1. YaccProduction : add length function to the class (ex. p$length() ).

  2. Implement tracking. YaccProduction new methods: p$linespan(n) and p$lexspan(n).

  3. Implement error recovery.

#### BUG FIXES

  1. Lex is duplicating numeric tokens.


### Changes in v1.0.3

#### NOTES

  1. Use CRAN template of MIT license.

  2. Add author of PLY python library as one of the authors of this extension.

  3. Add BSD license of PLY to file inst/COPYRIGHTS.

### Changes in v1.0.2

#### BUG FIXES

  1. Fix DESCRIPTION due to 'Non-FOSS package license (file LICENSE)' message.

#### NOTES

  1. Replace .Internal(inspect) function with C code.

### Changes in v1.0.1

#### BUG FIXES

  1. Library not working on 64-bit R.
