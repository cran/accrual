accrual.plot.multicenter<-function(n,TT,P,J,Tm,Tsj,m,all){
  nmax=2*n
  accrual.multi.out=accrual.multi.n(n,TT,P,J,Tm,Tsj,m,Tpred=TT,all)
  dist.n=accrual.multi.out[[2]]
  dist.n.pred=quantile(dist.n, prob=c(0.025, 0.5, 0.975))
  site.pred=accrual.multi.out[[1]]

  # Save current graphical parameters
  oldpar <- par(no.readonly = TRUE)
  # set up on.exit to restore graphical parameters when the function exits
  on.exit(par(oldpar))


  layout(matrix(c(2,2,2,1),nrow=1))
  par(mar=c(4.1,0.1,2.1,0.1))
  dist.n=accrual.multi.out[[2]]
  accrual.hist <-cut(dist.n,seq(0,nmax,length=40))
  barplot(table(accrual.hist),horiz=TRUE,
          axes=FALSE,xlab=" ",ylab=" ",space=0,
          col="white",names.arg=rep(" ",39))
  par(mar=c(4.1,4.1,2.1,0.1))
  plot(c(-0.5,TT),c(0,nmax),xlab="Time (Months)",
       ylab="Number of participants",type="n",xaxt = 'n')
  axis(1, at=seq(0,TT,6))
  legenda=paste("Total participants needed:", n)
  legendb=paste("Expected total months to complete:",TT)
  legendd=paste("Time to now:", Tm)
  legendc=paste("Participants recruited:",sum(m))
  no.site=dim(site.pred)[2]
  legende=paste("Number of sites for prediction:",no.site)
  legendf=paste("Participants in", TT, "months:",dist.n.pred[2],"(",round(dist.n.pred[1]),",",round(dist.n.pred[3]),")" )

  legend(-0.5, nmax, legend=c("Input Information:",
                              legenda,legendb,legendc,legendd,legende,"------------------------","Summary of results:",legendf))

  tlist <- Tm:TT
  mid=lcl=ucl=rep(NA,length(tlist))
  for (i in 1:length(tlist)){
    Ti=tlist[i]
    accrul.multi=quantile(accrual.multi.n(n,TT,P,J,Tm,Tsj,m,Ti,all)[[2]],prob=c(0.025,0.5,0.975))
    lcl[i]=accrul.multi[1]
    mid[i]=accrul.multi[2]
    ucl[i]=accrul.multi[3]
  }
  polygon(c(tlist,rev(tlist)),c(lcl,rev(ucl)),density=-1,col="gray",border=NA)
  lines(tlist,mid,col="white")

  ## codes for piece wise accrual projection
  mnew=m[order(match(Tsj,m))]
  Tsjnew=sort(Tsj)
  tlab0=0
  mlab0=0
  for (j in 1:J){
    if (Tsjnew[j]<=Tm){
      tlab1=Tsjnew[j]
      mlab1=sum(mnew[1:j])*(Tsjnew[j]-tlab0)/Tm +mlab0
      segments(tlab0,mlab0,tlab1,mlab1)
      tlab0=tlab1
      mlab0=mlab1}
  }
  segments(tlab0,mlab0,Tm,sum(m))
  lines(rep(n,TT),col="red")
  return(list(site.pred,dist.n.pred))
}
