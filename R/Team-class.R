## Team

.Team <- setRefClass("Team",
    fields = list(parallelParam = "ParallelParam"),
    contains = "Consumer")

## MulticoreTeam

.mccollect <- 
    function (jobs, wait = TRUE, timeout = 0, intermediate = FALSE)
{
    if (is(jobs, "process"))
        jobs <- list(jobs)
    results <- vector("list", length(jobs))
    t0 <- as.numeric(Sys.time(), units="secs") + timeout
    for (i in seq_along(jobs)) {
        result <- .mc_collect(jobs[[i]], wait, timeout, intermediate)
        if (is.null(result))
            results[i] <- list(NULL)
        else results[[i]] <- result[[1]]
        timeout <- max(0L, t0 - as.numeric(Sys.time(), units="secs"))
    }
    names(results) <- names(jobs)

    if (all(sapply(results, is.null)))
        results <- NULL
    else 
        names(results) <- sapply(jobs, function(elt) {
            if (is.null(nm <- elt$name))
                nm <- elt$pid
            nm
        })
    results
}

.MulticoreTeam <- setRefClass("MulticoreTeam",
    fields=list(
      tasks="list",          # status: IDLE, YIELD, VALUE, ERROR, DONE
      FUN="function",
      .id="integer", .yid="integer"),
    contains = "Team",
    methods = list(
      initialize = function(...) {
          callSuper(..., .id=1L, .yid=1L)
      },

      names = function() sapply(tasks, "[[", "name"),
      status = function() sapply(tasks, "[[", "status"),
      idle = function() status() == "IDLE",
      yielding = function() status() == "YIELD",
      valued = function() status() == "VALUE",
      done = function() any(status() == "DONE"),

      .idx = function() which.max(idle()),
      .yidx = function() if (.yid == 1L) 1L else match(.yid, names()),

      consume = function(value) {
          if (verbose)
              message("Team$consume .id: ", .id)
          force(value)
          idx <- .idx()
          if (length(value) == 0L) {
              task <- .self$tasks[[idx]]
              task[c("name", "result", "status")] <-
                  list(.id, value, "DONE")
          } else if (is(value, "try-error")) {
              task <- .self$tasks[[idx]]
              task[c("name", "result", "status")] <-
                  list(.id, value, "ERROR")
          } else {
              task <- .mc_parallel(FUN(value), .id,
                                   parallelParam$mc.set.seed,
                                   parallelParam$verbose)
              task$status <- "YIELD"
          }
          .self$tasks[[idx]] <- task
          .self$.id <- .id + 1L
          .self
      },

      collect = function() {
          if (verbose)
              message("Team$collect .id: ", .id)
          if (!any(yidx <- yielding()))
              return(.self)
          results <- .mccollect(tasks[yidx], wait=FALSE)
          if (is.null(results))
              return(.self)
          idx <- !sapply(results, is.null)
          results <- results[idx]
          nms <- base::names(results)
          tids <- match(nms, names())
          tryCatch(.mc_kill(tasks[tids], 15), error = function(...) NULL)
          for (i in seq_along(nms)) {
              tid <- tids[i]
              task <- tasks[[tid]]
              task$result <- results[[i]]
              task$status <- "VALUE"
              .self$tasks[[tid]] <- task
          }
          .self
      },

      yield = function() {
          if (verbose)
              message("Team$yield .id: ", .id)
          yidx <- .yidx()
          while (tasks[[yidx]]$status %in% c("IDLE", "YIELD")) {
              collect()
              for (i in seq_len(sum(idle())))
                  consume(callSuper())
          }
          task <- tasks[[yidx]]
          .self$.yid <- .yid + 1L
          if (task$status != "DONE")
              .self$tasks[[yidx]]$status <- "IDLE"
          if (!done())
              consume(callSuper())
          task$result
      },

      show = function() {
          cat("MulticoreParam:\n")
          parallelParam$show()
          cat("\ntasks status:\n")
          print(noquote(setNames(status(), names())))
      }))
