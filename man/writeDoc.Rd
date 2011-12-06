\name{writeDoc}
\alias{writeDoc}
\title{Write the generated documentation to file(s)}
\description{
  This generic function enables one to write
  Rd documentation generated from a WADL document
  via \code{\link{makeWADLDocs}} or \code{\link{makeWADLDoc}}
  to a file or any connection.
}
\usage{
writeDoc(obj, to = stdout(), ...)
}
\arguments{
  \item{obj}{the documentation content to be written}
  \item{to}{the name of a file or a directory.}
  \item{\dots}{additional parameters for methods to customize}
}

\value{
  This is used for its side effeect.
}
\references{
}
\author{
Duncan Temple Lang
}
\seealso{
 \code{\link{makeWADLDocs}}
}
\examples{
}
\keyword{help}
\keyword{programming}

