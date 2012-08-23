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

## connect

connect <-
    function(blocks, df) 
{
    inUse <- sapply(blocks, function(x) all(x$inUse))
    cls <- sapply(blocks, class)
    if (any(inUse)) {
        msg <- sprintf("%s: already in use in another stream",
                       paste(cls[which(inUse)], collapse = ", "))
        stop(msg)
    }

    len <- length(blocks)
    n <- nrow(df)
    df$weight <- 1L
    
    for (i in seq_len(n)) 
    {
        fromi <- as.character(df$from[i])
        toi <- as.character(df$to[i])
        left <- blocks[[fromi]]
        right <- blocks[[toi]]
        if (is(right, "YConnector"))
            right$.upstream[[fromi]] <- left

        if (is(left, "TConnector")) 
        {
            orig <- left$.tOuts 
            temp <- TOut()
            temp$inputPipe <- left
            right$inputPipe <- temp
            left$.tOuts <- c(left$.tOuts, temp)
        } else {
            right$inputPipe <- left
        }
        
        if (is(right, "ParallelConnector")) {
            right$.upstream <- .mc_parallel(quote({
                while(TRUE) {
                    prime <- yield(left)
                    .mc_sendMaster(prime)
                }}))

        }
    }
    
    nms <- local({
        g <- graphBAM(df, edgemode="directed")
        outDeg <- degree(g)$outDegree
        names(outDeg[outDeg==0])
    })
    s <- lapply(blocks[nms], stream)
    for (i in seq_len(len))
        blocks[[i]]$inUse <- TRUE
    s
}


