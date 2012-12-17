.Seq <- setRefClass("Seq",
    fields = list(
      .start="ANY",
      from="ANY", to="ANY", by="ANY", yieldSize="integer"),
    contains="Producer",
    methods = list(
      initialize = function(..., yieldSize=1L) {
          ## increment yieldSize to avoid repeated addition
          callSuper(..., yieldSize=as.integer(yieldSize) + 1L)
      },
      reset = function() {
          from <<- .start
          invisible(.self)
      },
      yield = function() {
          if ((from - to) * by > 0)
              return(numeric())
          s <- seq(from, by=by, length.out=yieldSize)
          from <<- s[length(s)]
          s <- s[-length(s)]
          s[s <= to]
      },
      show = function() {
          cat("from:", from, "\nto:", to, "\nby:", by,
              "\nyieldSize:", yieldSize - 1L, "\n")
      }))

Seq <-
    function(from=1L, to=.Machine$integer.max, by=1L, yieldSize=1L,
             ...)
{
    yieldSize <- as.integer(yieldSize)
    .Seq$new(.start=from, from=from, to=to, by=by,
             yieldSize=yieldSize, ...)
}
