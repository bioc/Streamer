## ParallelParam

.ParallelParam <- setRefClass("ParallelParam",
    fields = list(size = "integer"),
    contains = "Streamer",
    methods = list(
      initialize = function(...) {
          callSuper(...)
          if (length(size) != 0 && size < 1L)
              stop("'size' must be >= 1")
          .self
      },
      show = function() {
          cat("class:", class(.self), "\n")
          cat("size:", size, "\n")
          cat("verbose:", verbose, "\n")
      }))

.MulticoreParam <- setRefClass("MulticoreParam",
    fields = list(mc.set.seed = "logical"),
    contains = "ParallelParam",
    methods = list(
      show = function() {
          callSuper()
          cat("mc.set.seed:", mc.set.seed, "\n")
      }))

MulticoreParam <-
    function(size = getOption("mc.cores", 2L), mc.set.seed = TRUE,
             ...)
{
    .MulticoreParam$new(size=as.integer(size),
                        mc.set.seed=mc.set.seed, ..., inUse=TRUE)
}

## ParallelRegister

.parallelRegister <- setRefClass("ParallelRegister",
    fields = list(param = "ParallelParam"),
    methods = list(
      register = function(param) {
          oparam <- param
          if (is.null(param))
              .self$param <- .ParallelParam$new()
          else
              .self$param <- param
          invisible(oparam)
      },
      show = function() {
          if (param$inUse)
              param$show()
          else
              cat("<empty>\n")
      }))$new()                         # singleton

register <- function(param)
{
    if (missing(param))
        .parallelRegister
    else 
        .parallelRegister$register(param)
}
