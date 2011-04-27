.Stream <- setRefClass("Stream", contains = "Consumer")

.Stream$methods(
    yield=function()
    {
        .self$inputPipe$yield()
    
    })

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
