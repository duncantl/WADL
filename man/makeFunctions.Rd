\name{makeFunctions}
\alias{makeFunctions}
\title{Generate R functions to interface to WADL methods}
\description{
  The \code{makeFunctions} function takes methods
  described in a WADL and generates the R code to define
  R functions that can be used to invoke these REST methods.
}
\usage{
makeFunctions(wadl, methods = wadlMethods(wadl), eval = FALSE,
              rewriteURL = function(x) x,
              makeFun = makeFunction, ...)
}
\arguments{
  \item{wadl}{the WADL defining the the methods and resources.
    This can be the name of a file/URL, a parsed XML document or an
    object of class \code{\link{WADL-class}}.}
  \item{methods}{the methods that are to be converted to R functions}
  \item{eval}{a logical or environment controlling whether the R
    functions that are constructed as text are to be defined as regular
    R functions.  If this is a logical and \code{FALSE}, the code for
    the functions is returned. If this is \code{TRUE}, we define the
    functions in the global environment. If this is an environment, we
    define the functions in that environment.}
  \item{rewriteURL}{a function that takes a vector of URLs and rewrites
    them if necessary.  This can be specified as a character vector of
    length 2 and this is interpreted as being the first 2 arguments to
    \code{\link[base]{gsub}} and so are regular expressions. This allows
    for a simple way to rewrite the URLs.
  }
  \item{makeFun}{a function that is called to create the code for each
    REST method. This allows the caller to customize the code creation
    process.}
  \item{\dots}{additional arguments that are passed on to
    \code{makeFun}, e.g. \code{OptionsCharacterThreshold} for
    \code{makeFunction} which controls what the cut-off point is for the
    size (in bytes) of an options vector before we take it out of the
    function and make it into a separate variable.
  }
}
\details{

}
\value{

}
\references{

}
\author{

}
\note{

}


\seealso{

}
\examples{
gw = wadl(system.file("sampleWADLs", "GenesByMolecularWeight.wadl", package = "WADL"))
invisible(makeFunctions(gw, eval = TRUE,
                        rewriteURL = function(x)
                                        gsub("(/webservice/)", "/eupathdb/webservices/", x)))

tmp = GenesByMolecularWeight.json("Cryptosporidium parvum,Leishmania major,Toxoplasma gondii",
                                  `o-fields` = "gene_type,organism")

 if(require(RJSONIO)) {
      # convert the answer
   val = fromJSON(I(tmp))
 }

 tmp = GenesByMolecularWeight.xml("Cryptosporidium parvum,Leishmania major,Toxoplasma gondii",
                                  `o-fields`="gene_type,organism")
 if(require(XML))
   val = xmlParse(tmp, asText = TRUE)


  makeFunctions(gw, eval = TRUE, rewriteURL = c("(/webservice/)", "/eupathdb/webservices/"))
}
\keyword{programming}
\keyword{interface}

