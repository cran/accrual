accrual.multi.n <- function(n,TT,P,J,Tm,Tsj,m,Tpred,all){
  no.sim=1000
  delta.T=Tm-Tsj
  delta.T[delta.T<0]=0

  delta.Tpred=Tpred-Tsj
  delta.Tpred[delta.Tpred<0]=0

  r=n/J*P+m
  p=(TT*P+delta.T)/(TT*P+delta.Tpred)

  M=matrix(rep(m,each=no.sim),nrow=no.sim)
  ETAp=matrix(NA,nrow=no.sim,ncol=J)
  for (s in 1:J){

    ETAp[,s]=rnbinom(no.sim, size=r[s], prob=p[s])

  }

  ETA=M+ETAp

  if(all==TRUE){
    pred.each.site=apply(ETA,2,quantile, prob=c(0.025, 0.5, 0.975))
    dist.n=rowSums(ETA)
    output.n=list(pred.each.site,dist.n)
  } else {
    ETA.started=ETA[,(Tm-Tsj)>=0]
    if (sum((Tm-Tsj)>=0)==1){
      pred.each.site.started=quantile(ETA.started, prob=c(0.025, 0.5, 0.975))
    } else {
      pred.each.site.started=apply(ETA.started,2,quantile, prob=c(0.025, 0.5, 0.975))
    }
    dist.n.started=rowSums(ETA.started)
    output.n=list(pred.each.site.started,dist.n.started)}
  return(output.n)
}
