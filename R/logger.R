# MIT License
#
# Copyright (c) 2016 System in Cloud - Marek Jagielski
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#    
#    The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# This work is derived from PLY python library:
# -----------------------------------------------------------------------------
# ply: lex.py
#
# Copyright (C) 2001-2016
# David M. Beazley (Dabeaz LLC)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the name of the David Beazley or Dabeaz LLC may be used to
#   endorse or promote products derived from this software without
#  specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------

#' Print log message to file or console.
#' 
#' This object is a stand-in for a logging object created by the
#' logging module. RLY will use this by default to create things
#' such as the parser.out file. If a user wants more detailed
#' information, they can create their own logging object and pass
#' it into RLY.
#' ' 
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom futile.logger flog.error flog.warn flog.info flog.debug
#' @importFrom futile.logger flog.appender 
#' @importFrom futile.logger appender.console appender.file
#' @format A \code{\link{R6Class}} object
#' 
#' @export
#' 
#' @examples
#' debuglog <- RlyLogger$new(".", "file.out")
#' debuglog$info('This is info message')
RlyLogger <- R6Class("RlyLogger",
    public = list(
        name = NA,
        initialize = function(dir=NA, name=NA) {
          self$name <- randomString(4)
          if(is.na(name)) flog.appender(appender.console(), name=self$name)
          else            flog.appender(appender.file(name), name=self$name)
        },
        error = function(msg) { flog.error(msg, name=self$name) },
        warn  = function(msg) { flog.warn(msg, name=self$name) },
        info  = function(msg) { flog.info(msg, name=self$name) },
        debug = function(msg) { flog.debug(msg, name=self$name) }
    )
)

#' Null logger is used when no output should be generated. 
#'
#' Does nothing.
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @format A \code{\link{R6Class}} object
#' 
#' @export
#' 
#' @examples
#' debuglog <- NullLogger$new()
#' debuglog$info('This will not print')
NullLogger <- R6Class("NullLogger",
    public = list(
        initialize = function(f) {
        },
        error = function(msg) { },
        warn  = function(msg) { },
        info  = function(msg) { },
        debug = function(msg) { }
    )
)