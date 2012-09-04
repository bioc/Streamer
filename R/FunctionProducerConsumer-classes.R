## FunctionProducer

.FunctionProducer <- 
    setRefClass("FunctionProducer",
        contains = "Producer",
        fields = list(FUN = "function", RESET="function", state="ANY"))
 
.FunctionProducer$methods(
    reset = function()
    {
        "reset FunctionProducer"
        if (verbose) msg("FunctionProducer$reset")
        RESET(state)
    },
    yield = function()
    {
        "yield data from FunctionProducer"
        if (verbose) msg("FunctionProducer$yield")
        FUN()
    })

FunctionProducer <-
    function(FUN, RESET, ..., state = NULL)
{
    if (missing(FUN))
        FUN <- function() logical()
    if (missing(RESET))
        RESET = function(state) {}
    .FunctionProducer$new(FUN=FUN, RESET=RESET, state=state, ...)
}

## FunctionConsumer

.FunctionConsumer <- 
    setRefClass("FunctionConsumer",
        contains = "Consumer",
        fields = list(FUN = "function", RESET="function", state="ANY")) 
 
.FunctionConsumer$methods(
    reset = function()
    {
        "reset FunctionConsumer"
        if (verbose) msg("FunctionConsumer$reset")
        RESET(state)
    },
    yield = function()
    {
        "yield data from FunctionConsumer"
        if (verbose) msg("FunctionConsumer$yield")
        FUN(callSuper())
    })

FunctionConsumer <- function(FUN, RESET, ..., state=NULL)
{
    if (missing(FUN))
        FUN = function(y) y
    if (missing(RESET))
        RESET = function(state) {}
    .FunctionConsumer$new(FUN=FUN, RESET=RESET, state=state, ...)
}

