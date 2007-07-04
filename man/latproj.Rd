\name{latproj}

\alias{latproj}

\title{Project new nodes into a previously learned latent space}
\description{Project new nodes into a previously learned latent space. In this phase, only the latent positions of the new nodes are estimated. The learning nodes remain fixed in the latent space. The parameters are estimated using the maximum likelihood method via the simulated annealing optimizer in \link{optim}.}

\usage{latproj(prms, Y, ind, method="SANN")}

\arguments{
  \item{prms}{The parameters of the latent space learned with \link{latpos}}
  \item{Y}{The whole sociomatrix including the new nodes}
  \item{ind}{The list of test nodes}
  \item{method}{The optimization method used by \link{optim} to solve the ML estimation problem (optional)}
}

\value{
  \item{prms}{An LLN object}
  \item{prms\$alpha}{A copy of the value of the intercept parameter estimated during the learning phase.}
  \item{prms\$Z}{The latent positions of the learning nodes and the test nodes}
  \item{prms\$Y}{A copy of the sociomatrix associated with the studied network}
  \item{prms\$flag}{A vector of length equal to the number of node, specifying whether  each node was used to learn the network, or was a test node subsequently added to the network using "latproj" (1=learning node and 2=test node)}
  \item{prms\$optim}{The information returned by the optimization algorithm}
}

\author{Charles Bouveyron and Hugh Chipman <firstname.lastname@acadiau.ca>}

\references{
Charles Bouveyron and Hugh Chipman, A supervised latent classifier for graph-structured data with application to social network analysis, Technical Report, Acadia University, 2007.

Peter D. Hoff, Adrian E. Raftery and Mark S. Handcock. Latent space approaches to social network analysis. Journal of the American Statistical Association, 97, 460, pp. 1090-1098, 2002.
}

\seealso{\link{latpos}}

\examples{
# Type demo(LLN) for a complete demonstration
data(Monks)
learn <- seq(1,17); test <- c(18)
pos <- latpos(Monks$Y[learn,learn],p=2)
proj <- latproj(pos,Monks$Y,test)
plot(x=proj$Z,prms=proj,cls=Monks$cls)
}

\keyword{methods}
