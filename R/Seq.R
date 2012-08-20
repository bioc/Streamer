.Seq <- setRefClass("Seq",
    fields = list(
      from="numeric", to="numeric", by="numeric"),
    contains="Producer",
    methods = list(
      yield = function() {
          if ((from - to) * by > 0)
              return(integer())
          s <- seq(from, by=by, length.out=yieldSize + 1L)
          .self$from <- s[length(s)]
          s <- s[-length(s)]
          s[s <= to]
      },
      show = function() {
          cat("from:", from, "\nto:", to, "\nby:", by,
              "\nlength.out:", yieldSize, "\n")
      }))

Seq <-
    function(from=1L, to, by=1L, length.out=1L, ...)
{
    if (missing(to)) {
        to <-
            if (is.integer(from)) {
                .Machine$integer.max
            } else Inf
        if (by < 0)
            to <- -to
    }

    if (from > to)
        stop("'from' must be less than or equal to 'to'")
    length.out <- as.integer(length.out)
    .Seq$new(from=from, to=to, by=by, yieldSize=length.out, ...)
}
