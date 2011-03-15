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
    function(verbose=FALSE)
{
    .RawToChar$new(verbose=verbose)
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
    function(verbose=FALSE)
{
    .Rev$new(verbose=verbose)
}
