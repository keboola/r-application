#' Generic abstract class for an application. Provides basic functions
#'  like logging, working with environment and error handling
#' @import methods
#' @export Application
#' @exportClass Application
Application <- setRefClass(
    'Application',
    fields = list(
        debugMode = 'logical',
        # for wrapTryCatch
        hasFailed = 'logical',
        messages = 'list',
        warnings = 'list'
    ),
    methods = list(
        #' Constructor.
        #'
        #' @exportMethod
        initialize = function(debugMode = FALSE) {
            debugMode <<- debugMode
        },

        #' Constructor.
        #'
        #` @param debugMode Logical TRUE to turn debugging on.
        #' @exportMethod
        setDebugMode = function(debugMode) {
            debugMode <<- debugMode
        },

        #' Verify that a value is empty
        #' @param Character or number or vector of those.
        empty = function(obj) {
            if (class(obj) == 'character') {
                ret <- (nchar(obj) == 0)
            } else if (class(obj) == 'numeric') {
                ret <- obj == 0
            } else if (is.null(obj)) {
                ret <- TRUE
            } else if (is.na(obj)) {
                ret <- TRUE
            }
            if (length(ret) > 1) {
                # it was a vector, check if all values of the vector wer empty
                if (length(ret[ret == TRUE]) == length(ret)) {
                    ret <- TRUE
                } else {
                    ret <- FALSE
                }
            }
            ret
        },
        
        #' Get system environment variable
        #' @param name Name of the environment variable
        #' @return Value of the environment variable or null if it does not exist.
        #' @export
        getEnv = function(name) {
            value <- Sys.getenv(name)
            if (empty(value)) {
                ret <- NULL
            } else {
                ret <- value
            }
            ret
        },
                
        #' Log a debugging message
        #' @param msg Arbitrary msg or printable object
        #' @export
        logDebug = function(obj) {
            if (debugMode) {
                printLog(obj, 'stdout')
            }
        },
        
        #' Log a debugging message
        #' @param msg Arbitrary msg or printable object
        #' @export
        logInfo = function(obj) {
            printLog(obj, 'stdout')
        },
        
        #' Log a debugging message
        #' @param msg Arbitrary msg or printable object
        #' @export
        logError = function(obj) {
            printLog(obj, 'stderr')
        },

        #' Helper function to print timestamp with each message
        #' @param msg Arbitrary msg or printable object
        #' @param mode Logging mode either 'stdout' or 'stderr'
        #' @return void
        printLog = function(msg, mode = 'stdout') {
            printOut <- ""
            con <- textConnection("printOut", open = "w", local = TRUE)
            sink(con, type = c("output", "message"))
            if (is.character(msg)) {
                print(paste(format(
                    Sys.time(), "%Y-%m-%d %H:%M:%OS3"), ':',
                    msg))
            } else {
                print(format(
                    Sys.time(), "%Y-%m-%d %H:%M:%OS3"))
                print(msg)
            }
            sink(NULL, type = c("output", "message"))
            close(con)
            if (mode == 'stdout') {
                write(printOut, stdout())
            } else {
                write(printOut, stderr())
            }
        },

        #' Error handling wrapper which prints Java like stack trace in case of error.
        #' Comes from http://stackoverflow.com/a/24884348/41640
        #'
        wrapTryCatch = function(expr, silentSuccess = FALSE, stopIsFatal = TRUE) {
            hasFailed <<- FALSE
            messages <<- list()
            warnings <<- list()
            logger <- function(obj) {
                # Change behaviour based on type of message
                level = sapply(class(obj), switch, debug="DEBUG", message="INFO", warning="WARN", caughtError = "ERROR",
                               error=if (stopIsFatal) "FATAL" else "ERROR", "")
                level = c(level[level != ""], "ERROR")[1]
                simpleMessage = switch(level, DEBUG=,INFO=TRUE, FALSE)
                quashable = switch(level, DEBUG=,INFO=,WARN=TRUE, FALSE)
                
                # Format message
                time  = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
                txt   = conditionMessage(obj)
                if (!simpleMessage) txt = paste(txt, "\n", sep="")
                msg = paste(time, level, txt, sep=" ")
                calls = sys.calls()
                calls = calls[1:length(calls)-1]
                trace = limitedLabels(c(calls, attr(obj, "calls")))
                if (!simpleMessage && length(trace) > 0) {
                    trace = trace[length(trace):1]
                    msg = paste(msg, "  ", paste("at", trace, collapse="\n  "), "\n", sep="")
                }
                
                # Output message
                if (silentSuccess && !hasFailed && quashable) {
                    messages <<- append(messages, msg)
                    if (level == "WARN") warnings <<- append(warnings, msg)
                } else {
                    if (silentSuccess && !hasFailed) {
                        cat(paste(messages, collapse=""))
                        hasFailed <<- TRUE
                    }
                    cat(msg)
                }
                
                # Muffle any redundant output of the same message
                optionalRestart = function(r) { res = findRestart(r); if (!is.null(res)) invokeRestart(res) }
                optionalRestart("muffleMessage")
                optionalRestart("muffleWarning")
            }
            vexpr = withCallingHandlers(
                withVisible(expr),
                debug=logger, message=logger, warning=logger, caughtError=logger, error=logger
            )
            if (silentSuccess && !hasFailed) {
                cat(paste(warnings, collapse=""))
            }
            if (vexpr$visible) vexpr$value else invisible(vexpr$value)
        },

        ' Split a string into tokens using the given split character.
        #'
        #' @param string Arbitrary string.
        #' @param splitChar Split character.
        #' @param asLogical If true than a vector of TRUEs indexed by token name will be returned
        #'  if false (default) then a vector of tokens will be indexed.
        split = function(string, splitChar, asLogical = FALSE)
        {
            # split
            tokens <- strsplit(string, splitChar)[[1]]
            # trim whitespace from each item
            tokens <- lapply(tokens, function (x) {gsub("^\\s+|\\s+$", "", x)})                   
            if (asLogical) {
                logtokens <- logical()
                lapply(tokens, function (x) {logtokens[x] <<- TRUE})
                tokens <- logtokens
            }
            tokens
        },

        #' Main entry point
        run = function() {
            stop("Not implemented.")
        }
    )
)
