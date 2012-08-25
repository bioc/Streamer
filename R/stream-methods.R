.stream_set <- function(x, ..., verbose)
{
    ## helper used to construct streams
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

setMethod(stream, "Producer",
    function(x, ..., verbose=FALSE)
{
    if (0L == length(list(...)))
        .stream_set(x, verbose=verbose)
    else
        do.call(stream, c(rev(list(..., verbose=verbose)), list(x)))
})

setMethod(stream, "Consumer", .stream_set)
