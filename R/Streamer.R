.Streamer <- setRefClass("Streamer",
    fields = list(
      verbose="logical",
      inUse="logical"))

.Streamer$methods(
    initialize = function(..., verbose=FALSE)
    {
        "initialize 'Streamer'"
        invisible(callSuper(..., verbose=verbose, inUse=FALSE))
    },
    msg = function(fmt, ...)
    {
        "use 'fmt' to print ... as message()"
        message(sprintf(fmt, ...))
        invisible(.self)
    },
    reset = function()
    {
        "reset Streamer() -- noop"
        if (verbose) msg("Streamer$reset()")
        invisible(.self)
    },
    yield = function()
    {
        "yield default value list()"
        if (verbose) msg("Streamer$yield()")
        list()
    },
    status = function()
    {
        "report status of Steamer"
        if (verbose) msg("Streamer$status()")
        list(verbose=verbose, inUse=inUse)
    },
    show = function()
    {
        cat("class:", class(.self), "\n")
    })

setMethod(reset, "Streamer", function(x, ...) x$reset())

setMethod(yield, "Streamer", function(x, ...) x$yield())

setMethod(status,"Streamer", function(x, ...) x$status())
