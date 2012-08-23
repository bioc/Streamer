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
    function(X, FUN, ...)
{
    FUN <- match.fun(FUN)
    YIELD <- selectMethod("yield", class(X)) # avoid S4 dispatch

    it <- 0L
    result <- vector("list", 4096L)     # pre-allocate
    .partialResult <- function(err) {
        if (is(err, "simpleError")) {
            length(result) <- it
            err$message <- paste0("yield(): ", conditionMessage(err))
            err$partialResult <- result
            class(err) <- c("partialResult", class(err))
        }
        stop(err)
    }

    repeat {
        y <- tryCatch(YIELD(X), error = .partialResult)
        if (!length(y))
            break;
        y <- tryCatch(FUN(y, ...), error = .partialResult)
        it <- it + 1L
        if (it == length(result))       # grow
            length(result) <- 1.6 * length(result)
        result[[it]] <- y
    }
    length(result) <- it
    result
})

setMethod(sapply, "Producer",
    function(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)
{
    FUN <- match.fun(FUN)
    answer <- tryCatch(lapply(X = X, FUN = FUN, ...), error=function(err) {
        if (is(err, "partialResult"))
            err$partialResult <- simplify2array(err$partialResult,
                                                higher = (simplify == "array"))
        stop(err)
    })
    if (!identical(simplify, FALSE) && length(answer))
        simplify2array(answer, higher = (simplify == "array"))
    else answer
})
