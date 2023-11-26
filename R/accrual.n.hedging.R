accrual.n.hedging <-
function(n,TT,m,tm,Tp){
  S=1000
  Pgrid=seq(1:S)/S
  logPlike=(n*Pgrid)*log(TT*Pgrid)+lgamma(n*Pgrid+m)-(n*Pgrid+m)*log(TT*Pgrid+tm)-lgamma(n*Pgrid)
  Pdensity=exp(logPlike+100)/sum(exp(logPlike+100))
  Ppost=sample(Pgrid,S,prob=Pdensity,replace=TRUE)
  lambda=rgamma(S,shape=n*Ppost+m,rate=TT*Ppost+tm)
  simulated.n=rep(NA,ncol=S)
  for (i in 1:S) {
    enrollment=rpois(Tp-tm, lambda[i])
    simulated.n[i]=m+sum(enrollment)
  }
  P.hedging=quantile(Ppost,prob=c(0.025, 0.5, 0.975))
  TON.hedging=quantile(simulated.n, prob=c(0.025, 0.5, 0.975))
  return(list(TON.hedging,P.hedging,simulated.n))
}
