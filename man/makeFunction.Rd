\name{makeFunction}
\alias{makeFunction}
\title{Generate R code to interface to a WADL method}
\description{
  This function is the work-horse that generates the R code
  for a function with which we can interface to a
  REST method described by a WADL resource/method.
  This is repsonsible for creating the signature
  of the function, including the default values,
  and the body that invokes the method.
}
\usage{
makeFunction(params, url, options = params$options, name = "foo", converter = "NULL",
             action = "getForm",
             OptionsCharacterThreshold = getOption("WADLOptionsCharacterThreshold", 5000L),
             hooks = list(makeSignature = makeSignature, preCall = NULL, postCall = NULL))
}
\arguments{
  \item{params}{a description of the WADL method. This is a data frame
    with as many rows as there are parmeters in the WADL method.
   The columns are \code{name}, \code{type}, \code{required},
   \code{default} and \code{repeating}.
   This gives us all the information about each of the parameters}
  \item{url}{the URL for this method's resource}
  \item{options}{a list with information about each parameter}
  \item{name}{the name of the R function to be created}
  \item{converter}{an optional expression or symbol name that identifies
    the default converter, if any, that can process the result of the
    REST invocation and turn it into a more meaningful R object.}
  \item{action}{the name of the R function (in RCurl) to call
    when making the REST request. This is typically either "getForm" or
    "postForm".}
  \item{OptionsCharacterThreshold}{an integer value that controls how
    big the size of the permissible values vector for a parameter
    must be before we make it a global variable. This is to avoid
    issues with functions being too long to  read and maintain the
    source as text as an attribute.
  }
  \item{hooks}{a list with elements for any of the
    \code{makeSignature}, \code{preCall} and \code{postCall}.
    Each of these is a function that  is called during the construction
    of the code for the R function  to call the Web Service method.
    The preCall and postCall functions
    are passed a data frame describing the parameters, the URL of the
    method and the name of the method.
    The \code{makeSignature} function is called with the
    names of the parameters, the default values, a character
    vector of the additional arguments and their default values,
    the name of the method and the URL.
    The additional arguments are given as a named character vector
    with the names giving the parameter names and the values giving the
    default value. \code{...} is special and has no name.
  }
}
\details{
}

\value{
  If \code{eval} is \code{TRUE} or an environment, we define
  the function in that environment (or \code{globalenv}.
  If \code{eval} is \code{FALSE}, we return the text (character vector)
  containing the R code.
}
\references{
}
\author{
Duncan Temple Lang
}
\seealso{

}
\examples{
}
\keyword{programming}
\keyword{interface}
