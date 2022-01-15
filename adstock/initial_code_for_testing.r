pullDS<-function(Name=NULL,limiter=testing){

  conn<-odbcConnect('us-prd')

  out<-as.data.table(sqlQuery(conn,paste0('select * from ',Name,' ',limiter,';'),stringsAsFactors=FALSE,as.is=TRUE))

  odbcClose(conn)

  return(out)

}

 

calls<-pullDS(Name='sandbox_grwi.tl_calls_weekly', '' )

calls[,repcall:=detail_only+detail_sample]

setorder(calls,ccm_eid,wk_ending_dt)

 

calls[,repcallRM:=adstock(repcall,dcy=0.226219063,preserveSum = FALSE),by=ccm_eid]