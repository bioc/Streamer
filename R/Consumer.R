.Consumer <- setRefClass("Consumer",
    contains="Streamer",
    fields = list(inputPipe="ANY", .records="list"),     # actually, "Producer"
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
        idx <- seq_len(min(yieldSize, length(.records)))
        records <- .records[idx]
        .self$.records[idx] <- NULL
        records
    },
    status = function() 
    {
        "report status of 'Consumer'"
        if (verbose) msg("Consumer$status()")
        c(inputs=inputs(), callSuper())
    },
    .fill = function() {
        "fill stream with yieldSize records, if available"
        if(verbose) msg("Consumer$.fill()")
        while ( length(.records) < yieldSize &&
               0 != length(input <- inputPipe$yield()))
            .add(input)
        .self
    },
    .add = function(input)
    { 
        ".add (incomplete) 'input'"
        if (verbose) msg("Consumer$.add()")
        .self$.records <- c(.records, input)
        .self    
    })


setMethod(stream, "Consumer",
    function(x, ..., verbose=FALSE)
{  
    inp <- list(x, ...)
    use <- sapply(inp, function(k) {
                  k$inUse
               })
    cls <- sapply(inp, class)
    if(any(use)) {
        msg <- sprintf("%s : already in use in another stream",
                       paste(cls[which(use)], sep = " ", collapse = ", "))
        stop(msg)
    }
    x$inUse <- TRUE
    inputPipe <- Reduce(function(x, y) {
        x$inputPipe <- y
        y$inUse <- TRUE
        if ( is(x, "ParallelConnector")) {
            x$upstream <- parallel(quote({
                while(TRUE) {
                    prime <- yield(y)
                    sendMaster(prime)
            }}))
        }
        x
    }, list(x, ...), right=TRUE)
    .Stream$new(inputPipe=inputPipe, yieldSize=x$yieldSize, verbose=verbose)
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
