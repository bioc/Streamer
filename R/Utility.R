##
## Lightweight Consumer streams
##

.Utility <- setRefClass("Utility", contains = "Consumer")

## RawToChar

.RawToChar <- setRefClass("RawToChar",
    contains = "Utility",
    methods = list(
    yield = function()
    {  
        "convert list of raw() to char()"
         sapply(callSuper(), rawToChar)
    }))

RawToChar <-
    function(yieldSize=1e6, verbose=FALSE)
{
    .RawToChar$new(yieldSize=yieldSize, verbose=verbose)
}

## Rev

.Rev <- setRefClass("Rev",
    contains = "Utility",
    methods = list(
    yield = function()
    {
        rev(callSuper())
    }))

Rev <-
    function(yieldSize=1e6,verbose=FALSE)
{
    .Rev$new(yieldSize=yieldSize, verbose=verbose)
}
