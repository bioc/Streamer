.TOut <-
    setRefClass("TOut", contains="Consumer",
        fields=list(
            .start="numeric"
                ))

.TOut$methods(
    initialize =function(...) 
    {  
       "initialize TOut"
        callSuper(...)
        if (verbose) msg(".TOut$initialize()")
        .self$.start <- 1L
        .self$yieldSize <- yieldSize
        .self
    },
    yield = function() 
    { 
       "Calls yield on the inputPipe"
       if (verbose) msg(".TOut$yield()")
       while(length(.self$inputPipe$.records) - .self$.start < yieldSize &&
              0 != length(input <- .self$inputPipe$.fill()) )
         .self$inputPipe$.add(input)
       width <- min(yieldSize,  length(.self$inputPipe$.records))
       idx <- seq(.self$.start, .self$.start + width -1)
       .self$.start <- .self$.start + width
       dat <- .self$inputPipe$.records[idx]
       .self$inputPipe$.dump()
       dat
    })

TOut <- function( ..., yieldSize=1e6, verbose=FALSE) 
{
    .TOut$new(..., yieldSize=yieldSize, verbose = verbose)

}

.TConnector <- 
    setRefClass("TConnector", contains = "Consumer",
                fields = list(
                    .records = "list",
                    .downstream ="list",
                    .upstream="ANY"
                    ))
    
.TConnector$methods(
    initialize = function(upstream, downstream,..., verbose = FALSE) 
    {   
        "initialize TConnector"
        if (verbose) msg("TConnector$initialize")
        callSuper(...)
        if (verbose) msg(".TConnector$initialize()")
        .self$.downstream <- downstream
        .self$.upstream <- upstream
        .self$yieldSize <- yieldSize
        .self
    }, 
    .fill = function() 
    {   "Fills the stream with yieldSize records"
        if (verbose) msg("TConnector$.fill")
        .self$.upstream$yield()
    },
    .add = function(input)
    { 
        ".add (incomplete) 'input'"
        if (verbose) msg("TConnector$.add()")
        .self$.records <- c(.records, input)
        .self    
    },
    .dump = function() 
    { 
        "Clear .records that are used"
        if (verbose) msg("TConnector$.dump()")
        mn <- min(sapply(.self$.downstream, function(x) {
                    len <- length(x)
                    x[[len-2]]$.start
               }))
        if(mn !=1) {
            len <- length(.self$.downstream)
            for(i in 1:len) 
            {   
                pos <- length(.self$.downstream[[i]]) -2
                inp <-  .self$.downstream[[i]]
                count <- 1
                repeat {
                    inp <- inp$inputPipe
                    count <- count +1
                    if(count > pos) break
                }
                inp$.start <- inp$.start -mn +1
            }
            .self$.records[seq_len(mn-1)] <- NULL
        }
        .self
   }    
)                     

TConnector <- function(upstream, downstream, ..., yieldSize=1e6, verbose = FALSE) {
    ds <- list(downstream, ...)
    tconn <- Streamer:::.TConnector$new(upstream=upstream, downstream= ds, yieldSize = yieldSize)
    for(i in 1:length(ds))
    {
      inp <- ds[[i]]
      count <- 1
      len <- length(inp)
      repeat {
        inp <- inp$inputPipe
        count <- count +1 
        if(count >= len) break
      }
      ys <- inp$yieldSize  
      temp <- Streamer:::.TOut$new(yieldSize =ys)
      temp$inputPipe <- tconn

      inp <- ds[[i]]
      count <- 1
      len <- length(inp) 
      repeat {
        inp <- inp$inputPipe
        count <- count +1 
        if(count >= len) break
      }
      inp$inputPipe <- temp
    }
    tconn
}




