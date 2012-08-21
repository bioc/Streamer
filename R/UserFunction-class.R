.UserFunction <- 
    setRefClass("UserFunction",
        contains = "Consumer",
        fields = list(FUN = "function")) 
 
.UserFunction$methods(
    initialize = function(..., FUN)
    {
        "initialize myCons"
        callSuper(..., FUN=FUN)
    },
    yield = function()
    {
        "yield data from UserFunction"
        if (verbose) msg("UserFunction$yield")
        FUN(callSuper())
    })

UserFunction <- function(FUN, ...)
{
    .UserFunction$new(FUN=FUN,...)
}

