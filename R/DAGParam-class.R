.DAGParam <- setRefClass("DAGParam",
    fields = list( dag = "graphNEL" ),
    contains = "Streamer",
    methods = list(
      nodes = function() graph::nodes(dag),
      edges = function() graph::edges(dag),
      inEdges = function(node) graph::inEdges(dag)[[node]],
      show = function() {
          cat("class:", class(.self), "\n")
          cat("DAG nodes:", numNodes(dag), "edges:", numEdges(dag),
              "\n")
          cat("verbose:", verbose, "\n")
      }))
