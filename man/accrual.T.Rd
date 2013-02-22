\name{accrual.T}
\alias{accrual.T}
\title{
Prediction of Time.
}
\description{
Prediction of time frame for a certain nubmer of subjects.
}
\usage{
accrual.T(n, T, P, m,tm,Tmax)
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

 \item{Tmax}{
The max of x set up for the plot, default is 2*T.
}

}

\author{
Yu Jiang, Steve Simon, Matthew S. Mayo , Rama Raghavan, Byron J. Gajewski
}

\examples{
accrual.T(n=300, T=36, P=0.5, m=100, tm=10) 
}
\keyword{ accrual }
\keyword{ Bayesian }
