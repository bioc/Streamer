\name{Team-class}
\Rdversion{1.1}
\docType{class}
\alias{Team-class}
\alias{Team}

\title{Class \code{"Team"}}

\description{

  A \code{\linkS4class{Consumer}} to divide incoming tasks amongst
  processes for parallel evaluation; not supported on Windows.

}

\usage{Team(FUN, size=1L, mc.set.seed=TRUE, silent=TRUE, ...)}

\arguments{

  \item{FUN}{A \code{function} of one argument (the input to this
    consumer), to be applied to each element of the stream. The return
    value of the function is the value yield'ed.}

  \item{size}{The number of members in the team. If the upstream
    processing time is u unit of time, and \code{FUN} takes v units of
    time, then an efficient size is v / u or the number of cores -1,
    whichever is smaller.}

  \item{mc.set.seed, silent}{\code{logical(1)}; see
    \code{\link{mcparallel}}.}

  \item{...}{Additional arguments (e.g., \code{verbose}, passed to the
    \code{\linkS4class{Consumer}} constructor.}

}

\section{Constructors}{
  Use \code{Team} to contruct instances of this class.
}
\section{Fields}{
  Class-specific fields, intended for internal use:
  \describe{

    \item{\code{tasks}:}{Object of class \code{list} to contain
      in-process tasks.}

    \item{\code{FUN}:}{Object of class \code{function}, the function to
      be evaulated.}

    \item{\code{mc.set.seed, silent}:}{Object of class \code{logical},
      see \code{\link{mcparallel}}.}

    \item{\code{.id}:}{Object of class \code{integer}, a sequential
      identifier indexing tasks that have been consumed.}

    \item{\code{.yid}:}{Object of class \code{integer}, a sequential
      identifier indexing tasks that have been yielded.}

  }

  Inherited fields: see \code{\linkS4class{Consumer}}.

}

\section{Class-Based Methods}{

  Intended for internal use.
  \describe{

    \item{\code{initialize(...)}:}{Initialize the task pool and task id
    \code{.id} and yield id \code{.yid} indexes.}

    \item{\code{status()}:}{Status of each class; one of \sQuote{IDLE},
      \sQuote{YIELD} (computation in process), \sQuote{VALUE} (result
      available), \sQuote{ERROR}, and \sQuote{DONE} (end-of-stream
      recieved).}

    \item{\code{valued()}, \code{yielding()},
      \code{idle()}:}{\code{logical} vector indicating corresponding
      task status.}

    \item{\code{names()}:}{Task names, derived from \code{.id}.}

    \item{\code{consume(value)}:}{Create and fork a task to process
      \code{value}, obtained from the upstream yield. The task has
      \code{name} equal to \code{.id} and status \sQuote{YIELD};
      \code{.id} is incremented.}

    \item{\code{collect()}:}{Collect completed tasks, updating status to
      \sQuote{VALUE}, \sQuote{ERROR}, or \sQuote{DONE}.}

    \item{\code{yield()}:}{Return the next value, in the same order as
      upstream.}

    \item{\code{show()}:}{Display information on the team.}

  }

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{

  \code{\link{TConnector}} to apply \emph{different} functions to all
  elements of the team.

}

\examples{
team <- Team(function(x) { Sys.sleep(1); mean(x) }, size=5)
s <- stream(Seq(to=50, length.out=5), team)
system.time({
    while(length(y <- yield(s)))
        print(y)
})
}

\keyword{classes}
