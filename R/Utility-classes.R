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

RawToChar <- function(...)
    .RawToChar$new(...)

## Rev

.Rev <- setRefClass("Rev",
    contains = "Utility",
    methods = list(
    yield = function()
    {
       # rev(callSuper())
        sapply(callSuper(), rev)
    }))

Rev <- function(...)
    .Rev$new(...)
