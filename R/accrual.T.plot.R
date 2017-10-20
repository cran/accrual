accrual.T.plot <-
function(n,T,P,m,tm,np,Method) {
  nlist=seq(m+1,np)
  accrual.time=matrix(NA,nrow=length(nlist),ncol=3)
  for (i in 1:length(nlist)){
    npred=nlist[i]
    if (Method=="Informative Prior"){accrual.time[i,]=accrual.T.inform(n,T,P,m,tm,npred)[[1]]}
    if (Method=="Accelerated Prior"){accrual.time[i,]=accrual.T.inform(n,T,P=(1-m/n),m,tm,npred)[[1]]}
    if (Method=="Hedging Prior"){accrual.time[i,]=accrual.T.hedging(n,T,m,tm,npred)[[1]]}
  }
  
  
  if (Method=="Informative Prior"){accural.T.dist=accrual.T.inform(n,T,P,m,tm,np)[[2]]}
  if (Method=="Accelerated Prior"){accural.T.dist=accrual.T.inform(n,T,P=(1-m/n),m,tm,np)[[2]]}
  if (Method=="Hedging Prior"){accural.T.dist=accrual.T.hedging(n,T,m,tm,np)[[3]]
                               accrual.p.hedging=accrual.T.hedging(n,T,m,tm,np)[[2]]}
  
    
  ## Calcualte the number of subjects for T time
  if (Method=="Informative Prior"){accrual.n.T=accrual.n.inform(n,T,P,m,tm,T)[[1]]}
  if (Method=="Accelerated Prior"){accrual.n.T=accrual.n.inform(n,T,P=(1-m/n),m,tm,T)[[1]]}
  if (Method=="Hedging Prior"){accrual.n.T=accrual.n.hedging(n,T,m,tm,T)[[1]]}
  
  lclT=round(accrual.time[,1], 3)
  midT=round(accrual.time[,2], 3)
  uclT=round(accrual.time[,3], 3)
  
  
  layout(matrix(c(1,2,2,2)))
  par(mar=c(0.1,4.1,0.1,0.1))
  duration.hist <- cut(accural.T.dist,
                       seq(0,max(uclT)*1.2,length=40))
  barplot(table(duration.hist),horiz=FALSE,
          axes=FALSE,xlab=" ",ylab=" ",space=0,
          col="white",names.arg=rep(" ",39))
  
  par(mar=c(4.1,4.1,0.1,0.1))
  plot(c(0,max(uclT)*1.2),c(0,np),xlab="Time (Months)",
       ylab="Number of patients",xaxt = 'n',type="n")
  axis(1, at=seq(0,max(uclT)*1.2, 6)) 
  
  
  legenda=paste("Total targeted subjects:", n)
  legendb=paste("Total finish time (months):",T)
  legendd=paste("Time to date (months) :", tm)
  legendc=paste("Subjects recruited to date:",m)
  legende=paste("Subjects in", T, "months:",round(accrual.n.T[2]),"(",round(accrual.n.T[1]),",",round(accrual.n.T[3]),")" )
  legendf=paste("Time for",np, "subjects:", round(midT[np-m],digits=1),"(",round(lclT[np-m],digits=1),",",round(uclT[np-m],digits=1),")" )
  
  legend(max(uclT)*0.6, np*0.6, legend=c("Input Information:",
                                         legenda,legendb,legendc,legendd,"------------------------","Summary of Results:",legende,legendf))
  
  
  polygon(c(c(tm,lclT),rev(c(tm,uclT))),c(m:np,np:m),
          density=-1,col="gray",border=NA)
  lines(c(tm,midT),m:np,col="white")
  abline(v = T, col = "red") 
  segments(0,0,tm,m)
  return(list(paste("2.5%=",lclT[np-m]), paste("50%=",midT[np-m]),paste("97.5%=",uclT[np-m])))
  
}
