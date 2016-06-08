\name{accrual.T.plot}
\alias{accrual.T.plot}
\title{
Prediction of Time.
}
\description{
Prediction of time frame for a certain nubmer of subjects.
}
\usage{
accrual.T.plot(n, T, P, m,tm,np)
}
\arguments{
 \item{n}{
Total sample size
}
  \item{T}{
Total planned time
}
  \item{P}{
Research's confidence in the accrual process, range 0-1
}
  \item{m}{
Subject been recruited
}
  \item{tm}{
Time to now
}

 \item{np}{
The specific number of subjects want to be predicted
}

}

\author{
Yu Jiang, Cen Wu, Steve Simon, Matthew S. Mayo , Rama Raghavan, Byron J. Gajewski
}

\examples{
accrual.T.plot(n=300, T=36, P=0.5, m=100, tm=10, np=300) 
}
\keyword{ accrual }
\keyword{ Bayesian }
