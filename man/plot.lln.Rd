\name{plot.lln}

\alias{plot.lln}

\title{Plot tools designed for LLN objects}

\description{This function provides several tools for plotting networks: plot a simple network, plot a network with colored nodes according to their class membership, plot decision boundaries associated with a supervised classifier.}

\usage{plot.lln(x,...,cls,txt,classifier,unlab=FALSE)}

\arguments{
  \item{x}{The latent space parameters}
  \item{...}{Additional parameters}
  \item{cls}{the class membership labels of nodes used to determine colors in the plot (optional)}
  \item{txt}{the names of nodes (optinal)}
  \item{classifier}{the supervised classifier parameters (optional)}
  \item{unlab}{plot without colors the test nodes (optional)}
}

\author{Charles Bouveyron and Hugh Chipman <firstname.lastname@acadiau.ca>}

\seealso{\link{latpos}, \link{latproj}}

\examples{
# Type demo(LLN) for a complete demonstration
data(Monks)
pos <- latpos(Monks$Y,p=2)
plot(pos,cls=Monks$cls)
}

\keyword{ methods }
