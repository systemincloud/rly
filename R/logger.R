#' This object is a stand-in for a logging object created by the
#' logging module.   RLY will use this by default to create things
#' such as the parser.out file.  If a user wants more detailed
#' information, they can create their own logging object and pass
#' it into RLY.

RlyLogger <- R6Class("RlyLogger",
    public = list(
        name = NA,
        initialize = function(dir=NA, name=NA) {
          self$name <- randomString(4)
          if(is.na(name)) flog.appender(appender.console(), name=self$name)
          else            flog.appender(appender.file(name), name=self$name)
        },
        warn  = function(msg) { flog.warn(msg, name=self$name) },
        info  = function(msg) { flog.info(msg, name=self$name) },
        debug = function(msg) { flog.debug(msg, name=self$name) }
    )
)

#' Null logger is used when no output is generated. Does nothing.
NullLogger <- R6Class("NullLogger",
    public = list(
        initialize = function(f) {
        },
        warn  = function(msg) { },
        info  = function(msg) { },
        debug = function(msg) { }
    )
)