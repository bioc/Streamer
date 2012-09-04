setMethod(DAGParam, "missing",
    function(x, ...)
{
    .DAGParam$new(...)
})

setMethod(DAGParam, "graphNEL",
    function(x, ...)
{
    topology <- suppressWarnings(tsort(x))
    if (nodes(x) > 1L && !length(topology))
        stop("'x' is not a directed acyclic graph")
    .DAGParam$new(dag=x, ...)
})

setMethod(DAGParam, "matrix",
    function(x, W = NULL, V = NULL, ...)
{
    dag <- ftM2graphNEL(x, W=W, V=V, edgemode="directed")
    DAGParam(dag, ...)
})

setMethod(DAGParam, "data.frame",
    function(x, ...)
{
    nms <- c("From", "To", "W", "V")
    if (!all(c("From", "To") %in% names(x)))
        stop("'x' must have columns 'From', 'To'")
    ft <- cbind(x$From, x$To)
    DAGParam(ft, W=x$W, V=x$V, ...)
})
