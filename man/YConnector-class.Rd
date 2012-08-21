\name{YConnector-class}
\Rdversion{1.1}
\docType{class}
\alias{YConnector}
\alias{YConnector-class}

\title{Class "YConnector"}

\description{

  The \code{YConnector} \code{\linkS4class{Consumer}}-class can be used to 
  combine the output of multiple \code{stream}'s together. The output records of
  the \code{stream}'s are combined using a user supplied function (\code{fun}) 
  passed to the constructor of the \code{YConnector} class. The output of the
  \code{YConnector} can then be used to feed a \code{Consumer}-class connected 
  down-stream to it.

  The \code{YConnector} can be connector to other \code{Producer} and
  \code{Consumer} objects using the \code{\link{connect}} function.

}

\section{Methods}{
  Methods defined on this class include:
  \describe{

      \item{show}{\code{signature(object = "YConnector")}: Displays the
        names of the up-stream components to which the
        \code{YConnector}-class has been connected.}

  }
}

\usage{YConnector(fun, ...)}

\arguments{

   \item{fun}{A \code{function} that is used to combine the output of
     the streams connected up-stream to the \code{YConnector}. The
     function \code{fun} takes named arguments. The names correspond to
     the names of the objects passed to the \code{connect} function used
     to connect the \code{YConnector} to up-stream \code{Streamer}
     classes.}

  \item{...}{Arguments passed to the \code{\linkS4class{Consumer}}-class
    constructors.}

}


\section{Constructors}{
  Use \code{YConnector} to construct instances of this class.
}

\section{Fields}{
  \describe{

    \item{\code{.fun}:}{User defined \code{function} to combine the
      output of several streams. The function is applied on the named
      outputs obtained by calling the \code{yield} method on the named
      streams connected upstream to it.}

    \item{\code{.upstream}:}{A named \code{list} of objects connected
      up-stream to the \code{YConnector}-class. This field is meant to
      be internal to the class and is only modified by using the
      \code{connect} function to connect the \code{YConnector} to other
      \code{Streamer} objects.}

  }
}

\section{Methods}{There are no methods unique to this class.}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{\code{\link{stream}},\code{\link{TConnector}}, \code{\link{connect}} }

\examples{

fl <- system.file("extdata", "s_1_sequence.txt", package="Streamer")
#### Blocks for stream1 
b1 <- RawInput(fl, 100L, reader=rawReaderFactory(1e4))
c1 <- RawToChar()

#### Blocks for stream2
b2 <- RawInput(fl, 100L, reader=rawReaderFactory(1e4))
c2 <- RawToChar()
#### YConnector with function list for combining the blocks
y <- YConnector(fun=list)

blocks <- structure(list(b1,c1, b2, c2, y), 
                    names = c("b1", "c1", "b2", "c2","y"))
df <- data.frame(from =c("b1", "b2", "c1", "c2"), to = c("c1", "c2", "y", "y"))
#### Connect the blocks using the connect function
res <- connect(blocks, df)
y

#### Yield data from the y connector
yield(res$y)
}

\keyword{classes}

