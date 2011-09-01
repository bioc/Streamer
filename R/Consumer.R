.Consumer <- setRefClass("Consumer",
    contains="Streamer",
    fields = list(
        inputPipe="ANY", 
        .records="ANY",
        .bufferInt="BufferInt",
        .bufFun="logical"),     
    methods = list(
    initialize = function(..., inputPipe)
    {
        "initialize 'Consumer'"
        callSuper(..., .bufferInt=BufferInt(), .bufFun=FALSE)
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
        .fill()
        idx <- seq_len(min(yieldSize, .self$.bufferInt$length(.records)))
        records <- .self$.bufferInt$subset(.records,idx)
        .self$.records <- .self$.bufferInt$subset(.records,-idx)
        records
    },
    status = function() 
    {
        "report status of 'Consumer'"
        if (verbose) msg("Consumer$status()")
        c(recLength=.self$.bufferInt$length(.records), inputs=inputs(),
          callSuper())
    },
    .fill = function() {
        "fill stream with yieldSize records, if available"
        if(verbose) msg("Consumer$.fill()")
        if(!.self$.bufFun)
        {
            input <- inputPipe$yield()
            .self$.records <- new(class(input))
            .self$.bufferInt <- BufferInterface(input)
            .add(input)
            .self$.bufFun <- TRUE
        }
        while ( .self$.bufferInt$length(.records) < yieldSize &&
               0 != length(input<- inputPipe$yield())) 
        {
            .add(input)
        }
        .self
    },
    .add = function(input)
    { 
        ".add (incomplete) 'input'"
        if (verbose) msg("Consumer$.add()")
        .self$.records <- .self$.bufferInt$append(.records, input)
        .self    
    })

setMethod(stream, "Consumer",
    function(x, ..., verbose=FALSE)
{
    .stream_set(x, ..., verbose=verbose)
})

setMethod(show, "Consumer",
    function(object)
{
    callNextMethod()
    inp <- rev(object$inputs())
    indx = !inp %in%"TOut" 
    inp <- paste(inp[indx], collapse=" => ")
    txt <- sprintf("stream: %s", inp)
    cat(strwrap(txt, exdent=2), sep="\n")
})
