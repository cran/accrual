accrual.T.hedging <-
function(n,TT,m,tm,np){
  S=1000
  Pgrid=seq(1:S)/S
  logPlike=(n*Pgrid)*log(TT*Pgrid)+lgamma(n*Pgrid+m)-(n*Pgrid+m)*log(TT*Pgrid+tm)-lgamma(n*Pgrid)
  Pdensity=exp(logPlike+100)/sum(exp(logPlike+100))
  Ppost=sample(Pgrid,10000,prob=Pdensity,replace=TRUE)
  theta=1/rgamma(S,shape=n*Ppost+m,rate=TT*Ppost+tm)
  simulated.duration=rep(NA,S)
  for (i in 1:S) {
    wait=rexp(np-m,1/theta[i])
    simulated.duration[i]=tm+sum(wait)
  }
  P.hedging=quantile(Ppost,prob=c(0.025, 0.5, 0.975))
  TOT.hedging=quantile(simulated.duration, prob=c(0.025, 0.5, 0.975))
  return(list(TOT.hedging,P.hedging,simulated.duration))
}
