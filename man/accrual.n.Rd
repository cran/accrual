\name{accrual.n}
\alias{accrual.n}
\title{Prediction of Accrual in Fixed Time Frame
}
\description{
Produce a plot and output for prediction of the number of subjects can be recruited in a fixed time frame
}
\usage{
accrual.n(n, T, P, m, tm,nmax=2*n)
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

 \item{nmax}{
The max of y set up in the plot, the default is 2*n.
}

}


\author{
Yu Jiang, Steve Simon, Matthew S. Mayo , Rama Raghavan, Byron J. Gajewski
}

\examples{
 accrual.n(n=300, T=36, P=0.5, m=100, tm=10) 
}
\keyword{accrual}
\keyword{Byesian}
