.Stream <- setRefClass("Stream", contains = "Consumer")

.Stream$methods(
    yield=function()
    {
        .self$inputPipe$yield()
    
    })

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
            x$.upstream <- mcparallel(quote({
                while(TRUE) {
                    prime <- yield(y)
                    sendMaster(prime)
            }}))
        }
        x
    }, list(x, ...), right=TRUE)
    .Stream$new(inputPipe=inputPipe, verbose=verbose)
}

setMethod(length, "Stream",
    function(x)
{
    i <- 0L
    inp <- x
    while (extends(class(inp), "Consumer")) {
        inp <- inp$inputPipe
        i <- i + 1L
    }
    i
})

setMethod("[[", c("Stream", "numeric"),
    function(x, i, j, ...)
{
    i <- as.integer(i)
    len <- length(x)
    if (1L != length(i) || 0 >= i || len < i)
        stop("'i' must be integer(1), 0 < i <= length(x)")
    inp <- x$inputPipe
    while (extends(class(inp), "Consumer") && 1L < i) {
        inp <- inp$inputPipe
        i <- i - 1L
    }
    inp
})
