\name{accrual.plots}
\alias{accrual.plots}
\title{
Diagnostic Plots
}
\description{
The diagnostic panel shows four figures that help to understand the data distribution.
The top-left figure is the exponential quantile plot, which checks whether the distribution of waiting times is exponential.
The top-right figure is the histogram of the waiting times, with the red line is the theoretical exponential distribution.
The figure of waiting time verse cumulative accrual time is shown on the bottom left.
The figure of total accrual verse cumulative accrual time is shown on the bottom right.
}
\usage{
accrual.plots(w)
}
\value{
A set of figures on showing the data distribution of waiting time.
}
\arguments{
  \item{w}{
The accrual or waiting time dataset
}
}

\author{
Junhao Liu, Yu Jiang, Cen Wu, Steve Simon, Matthew S. Mayo, Rama Raghavan, Byron J. Gajewski
}

\examples{
accrual.plots(accrual.data)
}

\keyword{exponetial}
\keyword{Diagnostic}
