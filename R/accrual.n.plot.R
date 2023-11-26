accrual.n.plot <-
function(n,TT,P,m,tm,Tp,Method){
  tlist=seq(tm+1,Tp,length=200)
  accrual.count <- matrix(NA,nrow=200,ncol=3)
  for (i in 1:200) {
    time=tlist[i]
    if (Method=="Informative Prior"){accrual.count[i,]=accrual.n.inform(n,TT,P,m,tm,Tp=time)[[1]]}
    if (Method=="Accelerated Prior"){accrual.count[i,]=accrual.n.inform(n,TT,P=(1-m/n),m,tm,Tp=time)[[1]]}
    if (Method=="Hedging Prior"){accrual.count[i,]=accrual.n.hedging(n,TT,m,tm,Tp=time)[[1]]}
  }


  if (Method=="Informative Prior"){accural.n.dist=accrual.n.inform(n,TT,P,m,tm,Tp=Tp)[[2]]}
  if (Method=="Accelerated Prior"){accural.n.dist=accrual.n.inform(n,TT,P=(1-m/n),m,tm,Tp=Tp)[[2]]}
  if (Method=="Hedging Prior"){accural.n.dist=accrual.n.hedging(n,TT,m,tm,Tp=Tp)[[3]]
                               accrual.p.hedging=accrual.n.hedging(n,TT,m,tm,Tp=Tp)[[2]]}



  ## Calculate the duration for n participants

  if (Method=="Informative Prior"){accrual.T.n=accrual.T.inform(n,TT,P,m,tm,n)[[1]]}
  if (Method=="Accelerated Prior"){accrual.T.n=accrual.T.inform(n,TT,P=(1-m/n),m,tm,n)[[1]]}
  if (Method=="Hedging Prior"){accrual.T.n=accrual.T.hedging(n,TT,m,tm,n)[[1]]}

  lcln=accrual.count[,1]
  midn=accrual.count[,2]
  ucln=accrual.count[,3]

  # Save current graphical parameters
  oldpar <- par(no.readonly = TRUE)
  # set up on.exit to restore graphical parameters when the function exits
  on.exit(par(oldpar))

  layout(matrix(c(2,2,2,1),nrow=1))
  par(mar=c(4.1,0.1,2.1,0.1))
  accrual.hist <- cut(accural.n.dist,
                      seq(0,max(ucln)*1.2,length=40))
  barplot(table(accrual.hist),horiz=TRUE,
          axes=FALSE,xlab=" ",ylab=" ",space=0,
          col="white",names.arg=rep(" ",39))

  par(mar=c(4.1,4.1,2.1,0.1))
  plot(c(-0.5,Tp),c(0,max(ucln)*1.2),xlab="Time (Months)",
       ylab="Number of participants",type="n",xaxt = 'n')
  axis(1, at=seq(0, TT, 6))

  legenda=paste("Total targeted participants:", n)
  legendb=paste("Total completion time (Months):",TT)
  legendd=paste("Time to date (Months):", tm)
  legendc=paste("Participants recruited to date:",m)
  legende=paste("Participants in", Tp, "months:",round(midn[200]),"(",round(lcln[200]),",",round(ucln[200]),")" )
  legendf=paste("Time for",n, "participants:", round(accrual.T.n[2],digits=1),"(",round(accrual.T.n[1],digits=1),",",round(accrual.T.n[3],digits=1),")" )

  legend(-0.5, max(ucln)*1.2, legend=c("Input Information:",
                                       legenda,legendb,legendc,legendd,"------------------------","Summary of Results:",legende,legendf))
  polygon(c(c(tm,tlist),rev(c(tm,tlist))),c(c(m,lcln),rev(c(m,ucln))),
          density=-1,col="gray",border=NA)
  lines(c(tm,tlist),c(m,midn),col="white")
  segments(0,0,tm,m)
  lines(rep(n,200),col="red")
  return(list(paste("2.5%=",round(lcln[200])), paste("50%=", round(midn[200])),paste("97.5%=",round(ucln[200]))))
}
