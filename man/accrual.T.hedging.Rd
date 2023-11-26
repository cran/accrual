\name{accrual.T.hedging}
\alias{accrual.T.hedging}
\title{
Prediction of Time with Hedging Prior
}
\description{
Prediction of time frame with Hedging Prior for a certain number of participants.
}
\usage{
accrual.T.hedging(n, TT, m, tm, np)
}
\value{
A list of prediction on time frame that needed for reaching a certain number of participants with Hedging Prior.
}
\arguments{
  \item{n}{
Target sample size
}
  \item{TT}{
Target completion time
}
  \item{m}{
Sample observed to date
}
  \item{tm}{
Time to date
}
  \item{np}{
The specific number of participants want to be predicted
}
}

\author{
Junhao Liu, Yu Jiang, Cen Wu, Steve Simon, Matthew S. Mayo, Rama Raghavan, Byron J. Gajewski
}

\examples{
accrual.T.hedging(n=300, TT=36, m=100, tm=10, np=300)[[1]]
}

\keyword{accrual}
\keyword{Bayesian}
