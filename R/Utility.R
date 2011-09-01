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
       # rev(callSuper())
        sapply(callSuper(), rev)
    }))

Rev <-
    function(yieldSize=1e6,verbose=FALSE)
{
    .Rev$new(yieldSize=yieldSize, verbose=verbose)
}


connect <- function(blocks, df) 
{
    use <- sapply(blocks, function(x) {
        all(x$inUse)
    })
    cls <- sapply(blocks, class)
    if(any(use)) {
        msg <- sprintf("%s : already in use in another stream",
                       paste(cls[which(use)], sep = " ", collapse = ", "))
        stop(msg)
    }
    len <- length(blocks)
    n <- nrow(df)
    df <- cbind(df, "weight" =rep(1L, n))
    g <- graphBAM(df,edgemode="directed")
    outDeg <- degree(g)$outDegree
    nms <- names(outDeg[outDeg==0])
    for(i in 1:n) 
    {
        left <- blocks[[as.character(df$from[i])]]
        right <- blocks[[as.character(df$to[i])]]
        if(is(right, "YConnector")) {
            right$.upstream[[as.character(df$from[i])]] <- left
        }
        if(is(left, "TConnector")) 
        {
            orig <- left$.tOuts 
            temp <- TOut(yieldSize = left$yieldSize)
            temp$inputPipe <- left
            right$inputPipe <- temp
            left$.tOuts <- c(left$.tOuts, temp)
        } else {
            right$inputPipe <- left

        }
        if ( is(right, "ParallelConnector")) {
            right$.upstream <- multicore::parallel(quote({
                while(TRUE) {
                    prime <- yield(left)
                    sendMaster(prime)
        }}))

        }
    }
    s <- lapply(blocks[nms], stream)
    for(i in 1:len) 
    {
        blocks[[i]]$inUse <- TRUE
    }
    s
}


