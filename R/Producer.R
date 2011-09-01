.Producer <- setRefClass("Producer", contains="Streamer")

setMethod(stream, "Producer",
    function(x, ..., verbose=FALSE)
{
    if (0L == length(list(...)))
        .stream_set(x, verbose=verbose)
    else
        do.call(stream, c(rev(list(..., verbose=verbose)), list(x)))
})
