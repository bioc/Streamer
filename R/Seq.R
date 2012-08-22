.Seq <- setRefClass("Seq",
    fields = list(
      from="numeric", to="numeric", by="numeric", length.out="integer"),
    contains="Producer",
    methods = list(
      initialize = function(..., length.out) {
          ## increment length.out to avoid repeated addition
          callSuper(..., length.out=as.integer(length.out) + 1L)
      },
      yield = function() {
          if ((from - to) * by > 0)
              return(numeric())
          s <- seq(from, by=by, length.out=length.out)
          from <<- as.numeric(s[length(s)])
          s <- s[-length(s)]
          s[s <= to]
      },
      show = function() {
          cat("from:", from, "\nto:", to, "\nby:", by,
              "\nlength.out:", length.out - 1L, "\n")
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
    .Seq$new(from=from, to=to, by=by, length.out=length.out, ...)
}
