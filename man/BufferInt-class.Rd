\name{BufferInt-class}
\Rdversion{1.1}
\docType{class}
\alias{BufferInt-class}
\alias{BufferInt}
\alias{BufferInterface}
\alias{BufferInterface,ANY-method}
\alias{BufferInterface,data.frame-method}
\alias{show,BufferInt-method}

\title{Class "BufferInt"}

\description{

 An internal reference class container used by the \code{Consumer}-class to
 store functions that operate on the records stored in the \code{.records} field 
 of the \code{Consumer}-class.  Operations performed on 
 the \code{.records} field by the \code{Consumer}-class include \code{length}, 
 \code{append}, \code{subset}.  

 Users have the options of modifying the behaviour of the above mentioned 
 operations for records of different data types by declaring an S4 
 method \code{BufferInterface} that returns an object of \code{BufferInt}-class.
}

\section{Constructors}{
  Instances from this class are constructed with calls to \code{BufferInt}
  constructor.
}


\section{Fields}{
  \describe{
    \item{\code{length}:}{Object of class \code{function} that returns the length
        of the records.}
    \item{\code{append}:}{Object of class \code{function} that appends records
        together. This function is called when a new records is read and is to
        be added to the existing buffer. }
    \item{\code{subset}:}{Object of class \code{function} that subsets records.
    This function is called when records have been yielded and are to be removed
    from the buffer}
  }
}

\section{Methods}{
  Users have the option of controlling the beaviour of the functions
  \code{length}, \code{append} and \code{subset} used to manipulate the
  \code{.records} field of the \code{Consumer}-class by declaring a function
  \code{BufferInterface}.
  \describe{BufferInterface}{Returns an object of class \code{BufferInt} that
      holds functions for manipulating the record of the \code{Consumer} class.
  }  
}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{

  \code{\link{Streamer-package}}, \code{\linkS4class{Consumer}-class}.

}

\examples{
showClass("BufferInt")
selectMethod("BufferInterface", "data.frame")
}

\keyword{classes}
