pullDS<-function(Name=NULL,limiter=testing){

  conn<-odbcConnect('us-prd')

  out<-as.data.table(sqlQuery(conn,paste0('select * from ',Name,' ',limiter,';'),stringsAsFactors=FALSE,as.is=TRUE))

  odbcClose(conn)

  return(out)

}

datamax<-pullDS('sandbox_grwi.tl_promomix_forModel_plus', 'limit 100000')

datamax2 = datamax

datamax[,c("physician_id" ,"ecall_doc","ecall_t3")]<-NULL

datamax[,pi:=pi_medaler+pi_survey_invitation+pi_enewsletter]

datamax[,c("pi_medaler","pi_survey_invitation","pi_enewsletter")]<-NULL

#let's put a long halflife on spk (6?) and leave it at zero elsewhere

setorder(datamax,ccm_eid,wk_ending_dt)

adstock<-function(x,dcy=NULL,halflife=NULL,preserveSum=TRUE){

  #let dcy override halflife

  if(is.null(dcy) & !is.null(halflife)){

    dcy=1-0.5^(1/halflife)

  }



  if(is.null(dcy)){stop('adstock():Either dcy or halflife must have a non-Null value')}

  message(paste0('Adstocking with dcy=',dcy,' and preserveSum=',preserveSum,'.'))

  #works assuming x is a list of activity values sorted by date and returns

  # a list such that y[i]=(1-dcy)*x[i]+1-dcy*y[i-1], y[1]=dcy*x[1]

  y<-vector("numeric", length(x)[1])

  for (i in 1:length(x)[1]){

    if (i==1 ){y[i]<-x[i]}

    else {y[i]<-x[i]+y[i-1][[1]]*(1-dcy)}

  }



  #if preserveSum, multiply whole adstock series by dcy

  #if(preserveSum){ return(sapply(y,'*',dcy))}

  #else {return(unlist(y))}

  if(preserveSum){ return(y*dcy)}

  else {return(y)}

}
start_time <- Sys.time()

system.time(datamax[,spk6:=adstock(spk,halflife=6,preserveSum=FALSE),by=ccm_eid])

View(datamax)
print(Sys.time())

end_time <- Sys.time()

time1 = end_time - start_time
  print(time1)

library(compiler)

adstock_compiled<-cmpfun(adstock)