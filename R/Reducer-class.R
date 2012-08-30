.Reducer <- setRefClass("Reducer",
    fields = list(
      FUN = "function",
      init = "ANY",
      yieldNth = "integer",
      .hasInit = "logical",
      .curr = "ANY",
      .ith = "integer",
      .done = "logical"),
    contains = "Consumer",
    methods = list(

      .nth = function() length(.buf),

      done = function() .done,

      reset = function() {
          callSuper()
          .self$.curr <- NULL
          .self$.ith <- 0L
          .self$.done <- FALSE
      },

      yield = function() {
          if (verbose)
              message("Reducer$yield")
          if (done())
              return(.curr)
          repeat {
              val <- callSuper()
              if (!length(val)) {
                  ret <- if (.ith != 0) .self$.curr else val
                  .self$.done <- TRUE
                  .self$.curr <- val
                  return(ret)
              }
              .self$.curr <- 
                  if (.ith == 0L)
                      if  (.hasInit)
                          FUN(init, val)
                      else
                          val
                  else
                      FUN(.curr, val)
              .self$.ith <- .ith + 1L
              if (!is.na(yieldNth) && (.ith %% yieldNth) == 0) {
                  .self$.ith <- 0L
                  break
              }
          }
          .curr
      },

      show = function() {
          cat("yieldNth: ", yieldNth, " (current: ", .ith, ")\n",
              sep="")
          cat("has init:", .hasInit, "\n")
      }))

Reducer <-
    function(FUN, init,  ..., yieldNth=NA_integer_)
{
    FUN <- match.fun(FUN)
    hasInit <- !missing(init)
    if (!hasInit)
        init <- NULL
    .Reducer$new(FUN=FUN, init=init, yieldNth = as.integer(yieldNth),
                 ..., .done = FALSE, .ith = 0L, .hasInit = hasInit)
}
