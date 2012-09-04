.Consumer <- setRefClass("Consumer",
    contains="Streamer",
    fields = list(inputPipe = "ANY"),
    methods = list(
    initialize = function(..., inputPipe)
    {
        "initialize 'Consumer'"
        callSuper(...)
        if (verbose) msg("Consumer$initialize")
        if (!missing(inputPipe))
            .self$inputPipe <- inputPipe
        .self
    }))

.Consumer$methods(
    inputs = function()
    {
        "a character() describing this stream"
        if (verbose) msg("Consumer$inputs")
        inp <- .self
        pipe <- class(inp)
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
    },
    show = function() 
    {
        callSuper()
        inp <- rev(inputs())
        indx <- !inp %in%"TOut" 
        inp <- paste(inp[indx], collapse=" => ")
        txt <- sprintf("Stream: %s", inp)
        cat(strwrap(txt, exdent=2), sep="\n")
    })
