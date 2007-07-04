\name{Monks}

\alias{Monks}

\docType{data}

\title{The Sampson's Monks dataset}

\description{Sampson (1969) recorded the social interactions among a group of 18 monks while resident as an experimenter on vision, and collected numerous sociometric rankings.}

\usage{data(Monks)}

\format{
Monks$Y: the binary sociomatrix in which 1 indicates a friendship tie and 0 indicates no tie\\
Monks$cls: the class membership of each node: 1 corresponds to the \emph{loyal opposition} group, 2 corresponds to the \emph{young Turks} group and 3 corresponds to the \emph{outcasts} group.
}

\source{Sampson, S.~F. (1968), A novitiate in a period of change: An experimental and case study of relationships, Unpublished ph.d. dissertation, Department of Sociology, Cornell University.}

\references{White, H.C., Boorman, S.A. and Breiger, R.L. (1976). Social structure from multiple networks. I. Blockmodels of roles and positions. American Journal of Sociology, 81(4), 730-780.}

\examples{
data(Monks)
pos <- latpos(Monks$Y,p=2)
plot(x=pos$Z,prms=pos,cls=Monks$cls)
}

\keyword{datasets}
