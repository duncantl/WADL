\name{convertJSON}
\alias{convertJSON}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert JSON content to an R object
}
\description{
  This function is a simple interface to \code{\link[RJSONIO]{fromJSON}}
  which converts a string containing JSON (JavaScript Object Notation)
  content into an R object.
}
\usage{
convertJSON(text)
}
\arguments{
  \item{text}{a character string (vector of length 1) containing the
    JSON content.}
}

\value{
  An R object from converting the JSON content  to R.
}

\author{
%%  ~~who you are~~
}
\seealso{
 \code{\link[RJSONIO]{fromJSON}}
}
\examples{

}
\keyword{programming}
