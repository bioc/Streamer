.Consumer <- setRefClass("Consumer",
    contains="Streamer",
    fields = list(inputPipe="ANY"),     # actually, "Producer"
    methods = list(
    initialize = function(..., inputPipe, verbose=FALSE)
    {
        "initialize 'Consumer'"
        if (verbose) msg("Consumer$initialize")
        callSuper(..., verbose=verbose)
        if (!missing(inputPipe))
            .self$inputPipe <- inputPipe
        .self
    }))

.Consumer$methods(
    inputs = function()
    {
        "a character() describing this stream"
        if (verbose) msg("Consumer$inputs")
        pipe <- character(0)
        inp <- .self
        repeat {
            inp <- inp$inputPipe
            pipe <- append(pipe, class(inp))
            if (!extends(class(inp), "Consumer")) break
        }
        pipe
    },
    reset = function()
    {
        "delegate reset() to inputPipe"
        if (verbose) msg("Consumer$reset")
        inputPipe$reset()
    },
    yield = function()
    {
        "delegate yield() to inputPipe"
        if (verbose) msg("Consumer$yield()")
        inputPipe$yield()
    },
    status = function() 
    {
        "report status of 'Consumer'"
        if (verbose) msg("Consumer$status()")
        c(inputs=inputs(), callSuper())
    })

setMethod(stream, "Consumer",
    function(x, ..., verbose=FALSE)
{
    inputPipe <- Reduce(function(x, y) {
        x$inputPipe <- y
        x
    }, list(x, ...), right=TRUE)
    .Stream$new(inputPipe=inputPipe, verbose=verbose)
})

setMethod(show, "Consumer",
    function(object)
{
    callNextMethod()
    inp <- paste(object$inputs(), collapse=" << ")
    txt <- sprintf("stream: %s", inp)
    cat(strwrap(txt, exdent=2), sep="\n")
})
