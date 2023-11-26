accrual.gui <- function(){
  ## Gui for Accrual analysis                        ######
  accrualcallbackplot<- function(Targeted_sample_size,Targeted_finish_time_in_months,Your_confidence,
                                 Total_subjects_recruited_to_date,Time_to_date_in_months) {
    n=Targeted_sample_size
    TT=Targeted_finish_time_in_months
    P=Your_confidence
    m=Total_subjects_recruited_to_date
    tm=Time_to_date_in_months
    return <-accrual.n.plot(n,TT,P,m,tm,TT,Method="Informative Prior")
  }

  accrualcallbackplotadaptive<- function(Targeted_sample_size,Targeted_finish_time_in_months,Method,
                                         Total_subjects_recruited_to_date,Time_to_date_in_months){
    n=Targeted_sample_size
    TT=Targeted_finish_time_in_months
    m=Total_subjects_recruited_to_date
    tm=Time_to_date_in_months
    Method=Method
    return <-accrual.n.plot(n,TT,P=NULL,m,tm,TT,Method)
  }


  durationcallbackplot<- function(Targeted_sample_size,Targeted_finish_time_in_months,Your_confidence,
                                  Total_subjects_recruited_to_date,Time_to_date_in_months) {
    n=Targeted_sample_size
    TT=Targeted_finish_time_in_months
    P=Your_confidence
    m=Total_subjects_recruited_to_date
    tm=Time_to_date_in_months
    return <-accrual.T.plot(n,TT,P,m,tm,n,Method="Informative Prior")
  }

  durationcallbackplotadaptive<- function(Targeted_sample_size,Targeted_finish_time_in_months,Method,
                                          Total_subjects_recruited_to_date,Time_to_date_in_months){
    n=Targeted_sample_size
    TT=Targeted_finish_time_in_months
    m=Total_subjects_recruited_to_date
    tm=Time_to_date_in_months
    Method=Method
    return <-accrual.T.plot(n,TT,P=NULL,m,tm,n,Method)
  }


  diagexpgui <- function(w,Header) {
    if (Header==TRUE){ w <- as.matrix( read.csv(w),header=TRUE)}
    if (Header==FALSE){w <- as.matrix( read.csv(w),header=FALSE)}
    return(accrual.plots(w))

  }

  diagguiCallback <- function( arg ) {
    if( arg=="w" ) {
      datanames1 <- names( read.csv( guiGetValue("w") ) )
      print( datanames1 )
      guiSet( "datanames1", datanames1 )
    }

  }

  ## Need one multicallback function
  multiaccrualcallbackplot<- function(Total_smaple_size,Targeted_finish_time_in_months,Total_No_of_Site,
                                      Your_confidence, Subject_recruited_Multi,Total_months_after_started,Time_each_site_started_Multi,Use_all_sites) {
    n=Total_smaple_size
    TT=Targeted_finish_time_in_months
    J=Total_No_of_Site
    P=Your_confidence
    m=Subject_recruited_Multi
    Tm=Total_months_after_started
    Tsj=Time_each_site_started_Multi
    all=Use_all_sites
    return <- accrual.plot.multicenter(n,TT,P,J,Tm,Tsj,m,all)
  }


  mgui(accrualcallbackplot,title=c("SingleCenter","Subjective Prior","How many participants will you recruit?"),
       argSlider=list(Your_confidence=c(0,1,0.05)))


  mgui(durationcallbackplot,title=c("SingleCenter","Subjective Prior","How long will it take to reach the targeted sample size?"),
       argSlider=list(Your_confidence=c(0,1,0.05)))

  mgui(accrualcallbackplotadaptive,title=c("SingleCenter","Adaptive Prior","How many participants will you recruit?"),
       argOption=list(Method=c("Accelerated Prior","Hedging Prior")) )

  mgui(durationcallbackplotadaptive,title=c("SingleCenter","Adaptive Prior","How long will it take to reach the targeted sample size?"),
       argOption=list(Method=c("Accelerated Prior","Hedging Prior")) )


  mgui(multiaccrualcallbackplot,title=c("MultiCenter","How many participants will you recruit?"),
       argSlider=list(Your_confidence=c(0,1,0.05)),argOption=list(Use_all_sites=c("TRUE","FALSE")))


  mgui(diagexpgui,title=c("Diagnostic","Diagnostic Plots"),
       argFilename=list(w=NULL), callback=diagguiCallback, argOption=list(Header=c("TRUE","FALSE")) )
}
