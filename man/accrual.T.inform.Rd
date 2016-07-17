\name{accrual.T.inform}
\alias{accrual.T.inform}
\title{
Prediction of Time with Informative Prior
}
\description{
Prediction of time frame with Informative Prior for a certain nubmer of subjects.
}
\usage{
accrual.T.inform(n, T, P, m, tm, np)
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
  \item{np}{
The specific number of subjects want to be predicted
}
}

\author{
Junhao Liu, Yu Jiang, Cen Wu, Steve Simon, Matthew S. Mayo, Rama Raghavan, Byron J. Gajewski
}

\examples{
accrual.T.inform(n=300, T=36, P=0.5, m=100, tm=10, np=300)[[1]]
}

\keyword{accrual}
\keyword{Bayesian}
