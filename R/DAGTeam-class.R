.DAGTeam <- setRefClass("DAGTeam",
    fields = list(
      tbuf = "list",                    # results, in topological order
      dagParam = "DAGParam",
      teamParam = "ParallelParam"),
    contains = "Consumer",
    methods = list(
      reset = function() {
          callSuper()
          for (elt in tbuf)
              elt$reset()
      },
      yield = function() {
          values <- setNames(vector("list", length(tbuf)), names(tbuf))
          ## input value
          args <- callSuper()
          values[[1]] <- do.call(tbuf[[1]]$FUN, list(args))
          ## subsequent values
          for (nm in names(values)[-1]) {
              args <-values[dagParam$inEdges(nm)]
              values[[nm]] <- do.call(tbuf[[nm]]$FUN, args)
          }
          values[[length(values)]]
      }))

DAGTeam <-
    function(..., dagParam=DAGParam(), teamParam=MulticoreParam(1L))
{
    consumers <- list(...)
    if (!setequal(names(consumers), dagParam$nodes()))
        stop("'names(...)' and 'dagParam' nodes not equal")
    ok <- sapply(consumers, is, "FunctionConsumer")
    if (!all(ok))
        stop("'...' must be 'FunctionConsumer' instances, failing: '",
             paste(consumers[!ok], collapse="', '"), "'")

    ## re-order consumers into topological order
    tbuf <- consumers[tsort(dagParam$dag)]

    .DAGTeam$new(tbuf = tbuf, dagParam=dagParam, teamParam=teamParam)
}
