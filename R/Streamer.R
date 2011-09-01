.Streamer <- setRefClass("Streamer",
    fields = list(
      yieldSize="integer",
      verbose="logical",
      inUse="logical"))

.Streamer$methods(
    initialize = function(..., yieldSize=1e6, verbose=FALSE)
    {
        "initialize 'Streamer'"
        if (verbose) msg("Streamer$initialize")
        invisible(callSuper(..., yieldSize=as.integer(yieldSize),
                            verbose=verbose, inUse=FALSE))
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
        list(yieldSize=yieldSize, verbose=verbose, inUse=inUse)
    })

setMethod(reset, "Streamer", function(x, ...) x$reset())

setMethod(yield, "Streamer", function(x, ...) x$yield())

setMethod(status,"Streamer", function(x, ...) x$status())

setMethod(show, "Streamer",
    function(object)
{
    cat("class:", class(object), "\n")
})
