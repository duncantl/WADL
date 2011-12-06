\name{makeWADLDocs}
\alias{makeWADLDocs}
\alias{makeWADLDoc}
\title{Create the Rd files for methods in a WADL document}
\description{
  \code{makeWADLDocs} and the helper \code{makeWADLDoc}
  generate the different Rd files corresponding to groups
  of methods that share the same documentation.
  \code{makeWADLDocs} determines which methods reference
  the same method definition and use these references
  as aliases for the documentation.
  \code{makeWADLDoc} is used to generate the documentation
}
\usage{
makeWADLDocs(wadl, aliases = getWADLAliases(wadl), ..., makeDoc = makeWADLDoc,
             .genericParams = GenericParameterDocumentation)
}
\arguments{
  \item{wadl}{the WADL document given as a file/URL name, parsed XML
    document or a \code{WADL} object.}
  \item{aliases}{the collection of names of method that have an id,
    and another named character vector mapping the names of the methods
    that are references and reference the methods that have an id}
  \item{\dots}{additional parameters passed on to makeDoc}
  \item{makeDoc}{a function that is called to make a specific Rd file.
    It is called with the XML node for the method definition and a
    character vector naming the alias functions. This is a parameter so
    that others can customize the way the Rd files are generated.}
  \item{.genericParams}{a named list of strings providing documentation
    for parameters in the function being documented.
    This is used to document parameters that occur in multiple
    functions,    e.g. parameters we add to all functions for a
    particular Web Service.
  }
}
\value{
  An object of class WADLDocumentationCollection. This is
  a simple list. 
}
\references{
}
\author{
 Duncan Temple Lang
}

\seealso{
 \code{\link{writeDoc}}
}
\examples{

}
\keyword{programming}
\keyword{help}

