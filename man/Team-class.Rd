\name{Team-class}
\Rdversion{1.1}
\docType{class}
\alias{Team-class}
\alias{Team}

\title{Consumer class to enable parallel evaluation}

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
    \code{?mcparallel} on unix platforms.}

  \item{...}{Additional arguments (e.g., \code{verbose}, passed to the
    \code{\linkS4class{Consumer}} constructor.}

}

\section{Constructors}{
  Use \code{Team} to contruct instances of this class.
}

\section{Methods}{See \code{\link{Consumer}} Methods.}

\section{Internal Class Fields and Methods}{

  Internal fields of this class are are described with, e.g.,
  \code{getRefClass("Team")$fields}.

  Internal methods of this class are described with
  \code{getRefClass("Team")$methods()} and
  \code{getRefClass("Team")$help()}.

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{

  \code{\link{TConnector}} to apply \emph{different} functions to all
  elements of the team.

}

\examples{
if (.Platform$OS.type != "windows") {
    team <- Team(function(x) { Sys.sleep(1); mean(x) }, size=5)
    s <- stream(Seq(to=50, yieldSize=5), team)
    system.time({while(length(y <- yield(s)))
        print(y)
    })
}
}

\keyword{classes}
