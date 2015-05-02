.Stream_set <- function(x, ..., verbose)
{
    ## helper used to construct Streams
    inp <- list(x, ...)
    use <- sapply(inp, "[[", "inUse")
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
        if (is(x, "ParallelConnector")) {
            x$.upstream <- .mc_parallel(quote({
                while(TRUE) {
                    prime <- yield(y)
                    sendMaster(prime)
            }}))
        }
        x
    }, list(x, ...), right=TRUE)
    .Stream$new(inputPipe=inputPipe, verbose=verbose)
}

setMethod(Stream, "Producer",
    function(x, ..., verbose=FALSE)
{
    if (missing(...))
        .Stream_set(x, verbose=verbose)
    else
        do.call(Stream, c(rev(list(..., verbose=verbose)), list(x)))
})

setMethod(Stream, "Consumer", .Stream_set)
