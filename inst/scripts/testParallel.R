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
        Sys.sleep(2)
        callSuper()
    })

psleep <- .PSleeper$new()
csleep <- .CSleeper$new()
pconn <- ParallelConnector(streamer=psleep)
s <- stream(psleep, pconn ,csleep)
system.time(res <- yield(s)) ; res
