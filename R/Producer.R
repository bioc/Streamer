.Producer <- setRefClass("Producer", contains="Streamer")

setMethod(stream, "Producer",
    function(x, ..., verbose=FALSE)
{
    do.call(stream, c(rev(list(..., verbose=verbose)), list(x)))
})
