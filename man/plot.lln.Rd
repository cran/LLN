\name{plot.lln}

\alias{plot.lln}

\title{Plot tools designed for LLN objects}

\description{This function provides several tools for plotting networks: plot a simple network, plot a network with colored nodes according to their class membership, plot decision boundaries associated with a supervised classifier.}

\usage{plot.lln(x,...)}

\arguments{
  \item{x}{The latent positions of nodes}
  \item{...}{The additional parameters are 'prms', the latent space parameters, 'cls', the class membership labels of nodes, used to determine colors in the plot (optional), 'txt', the names of nodes (optinal), 'classifier', the supervised classifier parameters (optional), 'unlab', plot without colors the test nodes (optional)}
}

\author{Charles Bouveyron and Hugh Chipman <firstname.lastname@acadiau.ca>}

\seealso{\link{latpos}, \link{latproj}}

\examples{
# Type demo(LLN) for a complete demonstration
data(Monks)
pos <- latpos(Monks$Y,p=2)
plot(x=pos$Z,prms=pos,cls=Monks$cls)
}

\keyword{ methods }
