pullDS<-function(Name=NULL,limiter=testing){

  conn<-odbcConnect('us-prd')

  out<-as.data.table(sqlQuery(conn,paste0('select * from ',Name,' ',limiter,';'),stringsAsFactors=FALSE,as.is=TRUE))

  odbcClose(conn)

  return(out)

}

datamax<-pullDS('sandbox_grwi.tl_promomix_forModel_plus', '')

datamax2[,c("physician_id" ,"ecall_doc","ecall_t3")]<-NULL

datamax2[,pi:=pi_medaler+pi_survey_invitation+pi_enewsletter]

datamax2[,c("pi_medaler","pi_survey_invitation","pi_enewsletter")]<-NULL

#let's put a long halflife on spk (6?) and leave it at zero elsewhere

setorder(datamax2,ccm_eid,wk_ending_dt)

start_time_rcpp <- Sys.time()

adstock_c<-function(x,dcy=NULL,halflife=NULL,preserveSum=TRUE){

    library(Rcpp)

  #let dcy override halflife

  if(is.null(dcy) & !is.null(halflife)){

    dcy=1-0.5^(1/halflife)

  }



  if(is.null(dcy)){stop('adstock():Either dcy or halflife must have a non-Null value')}

  message(paste0('Adstocking with dcy=',dcy,' and preserveSum=',preserveSum,'.'))

  #works assuming x is a list of activity values sorted by date and returns

  # a list such that y[i]=(1-dcy)*x[i]+1-dcy*y[i-1], y[1]=dcy*x[1]

cppFunction('NumericVector adstock_loop(double dcy, NumericVector x, int iter) {
  int i;
  int n = iter;
  NumericVector y(iter);

   for( int i = 0; i < n; ++i ) {
           if (i==0 ){
                y[i]=x[i];
    }

    else {
                y[i]=x[i]+y[i-1]*(1-dcy);
        
    }
   }
  return y;
}')

iter = length(x)
y = adstock_loop(dcy, x, iter)


  #if preserveSum, multiply whole adstock series by dcy

  #if(preserveSum){ return(sapply(y,'*',dcy))}

  #else {return(unlist(y))}

  if(preserveSum){ return(y*dcy)}

  else {return(y)}

}

system.time(datamax2[,spk6:=adstock(spk,halflife=6,preserveSum=FALSE),by=ccm_eid])

end_time_rcpp <- Sys.time()

time2 = end_time_rcpp - start_time_rcpp
  print(time2)

  library(compiler)

adstock_c_compiled<-cmpfun(adstock)



benchmark(, 
    datamax[,spk6:=adstock_compiled(spk,halflife=6,preserveSum=FALSE),by=ccm_eid],
     datamax[,spk6:=adstock_c(spk,halflife=6,preserveSum=FALSE),by=ccm_eid], 
     datamax[,spk6:=adstock_c_compiled(spk,halflife=6,preserveSum=FALSE),by=ccm_eid],
          columns=c("test", "elapsed", "relative"),
          order="relative", replications=5000)


microbenchmark("adstock" = { datamax[,spk6:=adstock(spk,halflife=6,preserveSum=FALSE),by=ccm_eid] },
               "adstock_compiled" = { datamax[,spk6:=adstock_c(spk,halflife=6,preserveSum=FALSE),by=ccm_eid] },
               "adstock_c" = { datamax[,spk6:=adstock_c(spk,halflife=6,preserveSum=FALSE),by=ccm_eid]  },
               "adstock_c_compiled" = { datamax[,spk6:=adstock_c_compiled(spk,halflife=6,preserveSum=FALSE),by=ccm_eid]  })