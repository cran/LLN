\name{LLN-package}

\alias{LLN-package}

\alias{LLN}

\docType{package}

\title{Learning with latent networks}

\description{This is a new framework in which graph-structured data are used to train a classifier in a latent space, and then classify new nodes. During the learning phase, a latent representation of the network is first learned and a supervised classifier is then built in the learned latent space. In order to classify new nodes, the positions of these nodes in the learned latent space are
estimated using the existing links between the new nodes and the learning set nodes. It is then possible to apply the supervised classifier to assign each new node to one of the classes. This approach is based on the latent social network (hereafter LSN) model, introduced by Hoff, Raftery and Handcock (2002), which relates the presence of a tie between nodes $i$ and $j$ to the distance between them in latent space, \[ logit(P(Y_{ij}=1|\theta))=\alpha - \| Z_{i}-Z_{j}\|,\] where $logit(P)=\log(P/(1-P))$, $\theta=\{\alpha,Z\}$ represents all model parameters, $Z=\{Z_{i}\}_{i=1}^n$ are the $p$-dimensional latent coordinates associated to each node, where $p$ is unknown, and $\alpha$ determines the prior probability of an existing link between two nodes.  Nodes $i$ and $j$ have a high probability to be connected if $\alpha$ is large or if they are close in the latent space, \emph{i.e.}, $\| Z_{i}-Z_{j}\|$ is close to $0$. The parameters are estimated using the maximum likelihood method and the simulated annealing method is used to solve the optimization problem.\\\\ Notice that supervised learning with a continuous response would also be possible, as any supervised learning algorithm can be applied once the latent space is learned.}

\details{
\tabular{ll}{
Package: \tab LLN\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2007-06-25\cr
License: \tab GPL2.0\cr
}
The most important functions are: \link{latpos} and \link{latproj}
}

\author{Charles Bouveyron and Hugh Chipman <firstname.lastname@acadiau.ca>}

\references{
C. Bouveyron \& H. Chipman, A supervised latent classifier for graph-structured
data with application to social network analysis, Technical Report, Acadia University, 2007.

Peter D. Hoff, Adrian E. Raftery and Mark S. Handcock. Latent space approaches to social network analysis. Journal of the American Statistical Association, Dec 2002, Vol.97, Iss. 460; pp. 1090-1098.
}

\keyword{ package }

\seealso{\link{latpos}, \link{latproj}}

\examples{
# Type demo(LLN) for a complete demonstration
data(Monks)
learn <- seq(1,17); test <- c(18)
pos <- latpos(Monks$Y[learn,learn],p=2)
proj <- latproj(pos,Monks$Y,test)
plot(proj,cls=Monks$cls)
}
