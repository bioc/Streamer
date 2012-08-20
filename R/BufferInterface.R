.BufferInt <- setRefClass("BufferInt",
    fields=list(
        length="function",
        append="function",
        subset="function"))

.BufferInt$methods(
    initialize=function(length=.Primitive("length"),
                        append=.Primitive("c"), 
                        subset=.Primitive("["))
    {
        "initialize 'BufferInt'"
        .self$length <- length
        .self$append <- append
        .self$subset <- subset
        .self
    })

BufferInt <- function(length=.Primitive("length"), append=.Primitive("c"),
                     subset=.Primitive("["))
{
    .BufferInt$new(length=length, append=append, subset=subset)
}


setGeneric("BufferInterface",
    function(object) standardGeneric("BufferInterface"))

setMethod("BufferInterface", signature = signature(object = "ANY"),
    function(object) BufferInt())

setMethod("BufferInterface", signature = signature(object = "data.frame"),
    function(object)
{
    BufferInt(length=nrow, append=rbind, subset=function(x, i) { 
        as.data.frame(sapply(x, .Primitive("["), i))
    })
})



