library(Streamer)

.PSleeper <-
    setRefClass("PSleeper", contains="Producer",
                fields=list(tick="integer"))

.PSleeper$methods(
    initialize=function(..., tick=0L)
   {
       callSuper(...)
       .self$tick <- tick
       .self
   },
    yield=function() {
        .self$tick <- .self$tick + 1
        Sys.sleep(2)
        tick
    })

.CSleeper <-
    setRefClass("CSleeper", contains="Consumer")
 
.CSleeper$methods(
    yield=function() {
        Sys.sleep(1)
        callSuper()[[1]]
    })
pconn <- ParallelConnector()
psleep <- .PSleeper$new(yieldSize=1L)     ## sleeps for 2 s
csleep1 <- .CSleeper$new(yieldSize=1L)    ## sleeps for 1 s
csleep2 <- .CSleeper$new(yieldSize=1L)
csleep3 <- .CSleeper$new(yieldSize=1L)

## sequential

s <- stream(psleep, csleep1, csleep2, csleep3)
system.time(res <- yield(s)) ; res 


pconn <- ParallelConnector()
s1 <- stream(psleep,csleep1, csleep2, pconn, csleep3)
system.time(res <- yield(s1)) ; res 


s2 <- stream(psleep,csleep1, csleep2, pconn, csleep3)
system.time(res <- yield(s2)) ; res



fl <- system.file("extdata", "s_1_sequence.txt", package="Streamer")
b <- RawInput(fl, 100L, reader=rawReaderFactory(1e4))
pconn <- ParallelConnector()
s <- stream(RawToChar(), Rev(), pconn, b)
yield(s)

s <- stream(psleep, pconn)













