\name{accrual-package}
\alias{accrual-package}
\alias{accrual}
\docType{package}
\title{
Bayesian Accrual prediction
}
\description{
Description: Subject recruitment for medical research is challenging. Slow patient accrual leads to delay in research.Accrual monitoring during the process of recruitment is critical. Researchers need reliable tools to manage the accrual rate.We developed a Bayesian method that integrates researcher's experience on previous trials and data from the current study,providing reliable prediction on accrual rate for clinical studies.In this R package, we present functions for Bayesian accrual prediction which can be easily used by statisticiana and clinical researcher.s.
}
\details{
\tabular{ll}{
Package: \tab accrual\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-02-06\cr
License:\tab  GPL-2\cr 
}
There are major three funtions in the package. The accrual.gui function provide the gui version.
}
\author{
Yu Jiang, Steve Simon, Matthew S. Mayo , Rama Raghavan, Byron J. Gajewski

Maintainer:Yu Jiang <yjiang@kumc.edu>
}
\references{
Gajewski BJ, Simon SD, Carlson SE (2008). Predicting accrual in clinical trials with Bayesian posterior predictive distributions. Stat Med. 27(13):2328-40.
}
\keyword{accrual}
\keyword{Bayesian}
\examples{
accrual.n(n=300, T=36, P=0.5, m=100, tm=10)
accrual.T(n=300, T=36, P=0.5, m=100, tm=10)
accrual.plots(accrual.data)
accrual.gui()
}
