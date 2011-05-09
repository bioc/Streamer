.UserFunction <- 
    setRefClass("UserFunction",
        contains = "Consumer",
        fields = list(
            .fun= "function")) 
 
.UserFunction$methods(
    initialize = function(fun,...)
    {
        "initialize myCons"
        callSuper(...)
        if (.self$verbose)
            .self$msg("myCons$initialize")
        .self$.fun <- fun
        .self
    },
    yield = function()
    {
        "yield data from UserFunction"
        if (verbose) msg("UserFunction$yield")
        .self$.fun(callSuper())
    })

UserFunction <- function(fun, ..., yieldSize = 1e6, verbose = FALSE)
{
    .UserFunction$new(fun=fun,..., yieldSize=yieldSize, verbose=verbose)
}

