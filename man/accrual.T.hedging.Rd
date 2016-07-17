\name{accrual.T.hedging}
\alias{accrual.T.hedging}
\title{
Prediction of Time with Hedging Prior
}
\description{
Prediction of time frame with Hedging Prior for a certain nubmer of subjects.
}
\usage{
accrual.T.hedging(n, T, m, tm, np)
}
\arguments{
  \item{n}{
Target sample size
}
  \item{T}{
Target completion time
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
accrual.T.hedging(n=300, T=36, m=100, tm=10, np=300)[[1]]
}

\keyword{accrual}
\keyword{Bayesian}
