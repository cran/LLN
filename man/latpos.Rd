\name{lapos}

\alias{latpos}

\title{Use a binary sociomatrix to learn a latent space representation}

\description{Learn a latent space from a binary sociomatrix associated to the studied network. The latent social network (hereafter LSN) model, introduced by Hoff, Raftery and Handcock (2002), relates the presence of a tie between nodes $i$ and $j$ to the distance between them in latent space, \[ logit(P(Y_{ij}=1|\theta))=\alpha - \| Z_{i}-Z_{j}\|,\] where $logit(P)=\log(P/(1-P))$, $\theta=\{\alpha,Z\}$ represents all model parameters, $Z=\{Z_{i}\}_{i=1}^n$ are the $p$-dimensional latent coordinates associated to each node, where $p$ is unknown, and $\alpha$ determines the prior probability of an existing link between two nodes.  Nodes $i$ and $j$ have a high probability to be connected if $\alpha$ is large or if they are close in the latent space, \emph{i.e.}, $\| Z_{i}-Z_{j}\|$ is close to $0$. The parameters are estimated using the maximum likelihood method via the simulated annealing optimizer in \link{optim}.}

\usage{latpos(Y, p=2, alpha=0, method="SANN")}

\arguments{
  \item{Y}{The sociomatrix}
  \item{p}{The latent space dimension}
  \item{alpha}{An initial value for the intercept parameter in the logistic regression (optional, 0 by default)}
  \item{method}{The optimization method used by \link{optim} to solve the ML estimation problem (optional)}
}

\value{
  \item{prms}{An LLN object}
  \item{prms\$alpha}{The value of the intercept parameter used for learning the latent position of new nodes}
  \item{prms\$Z}{The latent positions of the learning nodes}
  \item{prms\$Y}{A copy of the sociomatrix associated with the network}   \item{prms\$flag}{A vector of length equal to the number of node, specifying whether  each node was used to learn the network, or was a test node subsequently added to the network using "latproj" (1=learning node and 2=test node)}
  \item{prms\$bic}{The BIC value associated with the learned latent configuration}
  \item{prms\$optim}{The information returned by the optimization algorithm}
}

\author{Charles Bouveyron and Hugh Chipman <firstname.lastname@acadiau.ca>}
\references{
Charles Bouveyron and Hugh Chipman, A supervised latent classifier for graph-structured data with application to social network analysis, Technical Report, Acadia University, 2007.

Peter D. Hoff, Adrian E. Raftery and Mark S. Handcock. Latent space approaches to social network analysis. Journal of the American Statistical Association, 97, 460, pp. 1090-1098, 2002.
}

\seealso{\link{latproj}}

\examples{
# Type demo(LLN) for a complete demonstration
data(Monks)
pos <- latpos(Monks$Y,p=2)
plot(x=pos$Z,prms=pos,cls=Monks$cls)
}

\keyword{ methods }