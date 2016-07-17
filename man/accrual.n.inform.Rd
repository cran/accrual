\name{accrual.n.inform}
\alias{accrual.n.inform}
\title{
Prediction of Accrual with Informative Prior in Fixed Time Frame
}
\description{
Produce an output for prediction of the number of subjects can be recruited in a fixed time frame with Informative Prior.
}
\usage{
accrual.n.inform(n, T, P, m, tm, Tp)
}

\arguments{
  \item{n}{
Target sample size
}
  \item{T}{
Target completion time
}
  \item{P}{
The prior certainty, range 0-1
}
  \item{m}{
Sample observed to date
}
  \item{tm}{
Time to date
}
  \item{Tp}{
The specific time that want to predict the recruitment
}
}

\author{
Junhao Liu, Yu Jiang, Cen Wu, Steve Simon, Matthew S. Mayo, Rama Raghavan, Byron J. Gajewski
}

\examples{
accrual.n.inform(n=300, T=36, P=0.5, m=100, tm=10, Tp=36)[[1]]
}

\keyword{accrual}
\keyword{Bayesian}
