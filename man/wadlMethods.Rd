\name{wadlMethods}
\alias{wadlMethods}
\title{Get descriptions of the methods from a WADL}
\description{
  This function extracts information from the WADL
  to describe each of the resources/methods.
}
\usage{
wadlMethods(doc, baseURL = xpathSApply(doc@ref, "//x:resources[@base]", xmlGetAttr, "base", namespaces = "x"))
}
\arguments{
  \item{doc}{the WADL as a URL/file name, parsed XML document or
    \code{\link{WADL-class}} object.}
  \item{baseURL}{the base URL for the resources in this WADL. This comes
  from the \code{resources} node}
}
\value{
  A list with as many elements as there are methods in the WADL.

  Each element is a data frame with as many rows as there are parameters
  in the method and containing the following columns
  \item{name}{the name of the parameters}
\item{type}{the type of the parameters as a character vector}
\item{required}{a logical vector indicating whether each parameter is
  required or optional}
\item{default}{a character vector providing the default value for each
  parameter, if available}
\item{repeating}{a logical vector indicating whether a parameter is
  allowed to have multiple values or is a scalar.}
\item{options}{a list containing a character vector for each parameter.
  The character vectors provide, if available, the set of permissible
  values for that parameter.}
}
\references{

}
\author{
Duncan Temple Lang
}

\seealso{
 \code{\link{wadl}}
}
\examples{

  gw = wadl(system.file("sampleWADLs", "GenesByMolecularWeight.wadl",  package = "WADL"))

  p = wadlMethods(gw)
  names(p)

  p[[1]]
}
\keyword{programming}
\keyword{interface}