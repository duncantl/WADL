\name{wadl}
\alias{wadl}
\alias{WADL-class}
\alias{names,WADL-method}
\alias{coerce,character,WADL-method}
\alias{coerce,XMLInternalDocument,WADL-method}
\alias{coerce,WADL,XMLInternalDocument-method}
\title{Create a WADL object}
\description{
  This is a constructor function that creates a \code{WADL}
  object which contains the parsed XML document for the WADL.

  Most of the functions allow us to specify the WADL
  by file name or URL, or XML document, or \code{WADL} object.
  If we work on the same WADL in numerous functions, it is best
  to parse it once and use the \code{WADL} object to avoid
  reading it each time.
}
\usage{
wadl(file)
}
\arguments{
  \item{file}{the URL or file containing the WADL information}
}
\value{
  An object of class \code{\link{WADL-class}}.
  This has a single slot named \code{ref} and this is a reference
  to the parsed XMLInternalDocument.
}
\references{
}
\author{
Duncan Temple Lang
}

\seealso{
}
\examples{
 gw = system.file("sampleWADLs", "GenesByMolecularWeight.wadl", package = "WADL")

 w = wadl(gw)

 wadlMethods(w)
 makeFunctions(w)

 eop = wadl("http://ops.epo.org/2.6.2/wsdl/ops.wadl")
}
\keyword{programming}
\keyword{interface}


