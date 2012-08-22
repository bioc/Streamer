.Producer <- setRefClass("Producer", contains="Streamer")

setMethod(stream, "Producer",
    function(x, ..., verbose=FALSE)
{
    if (0L == length(list(...)))
        .stream_set(x, verbose=verbose)
    else
        do.call(stream, c(rev(list(..., verbose=verbose)), list(x)))
})

setMethod(lapply, "Producer",
    function(X, FUN, ..., .env=parent.frame())
{
    FUN <- match.fun(FUN)
    fun <- function(yield) {
        y <- tryCatch(yield(), error=function(err) {
            stop("'lapply,Producer-method' yield() failed: ",
                 conditionMessage(err))
        })
        if (!length(y))
            return(y)
        tryCatch(eval(FUN(y, ...), .env), error=function(err) {
            stop("'lapply,Producer-method' FUN() failed: ",
                 conditionMessage(err))
        })
    }
    ## avoid S4 dispatch on yield(X)
    .Call(.lapply_Producer, fun, X$yield, environment())
})

setMethod(sapply, "Producer",
    function(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)
{
    FUN <- match.fun(FUN)
    .env <- parent.frame()
    answer <- lapply(X = X, FUN = FUN, ..., .env=.env)
    if (!identical(simplify, FALSE) && length(answer))
        simplify2array(answer, higher = (simplify == "array"))
    else answer
})
