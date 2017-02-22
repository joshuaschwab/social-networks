GetResults <- function(given_match){
  
  cat(date(),'started')
  #load(ff)
  
    ret.list <- list(
    threshold=sapply(given_match,function(ll)   tryCatch(ll$threshold, error=function(e) 0)),
    dedupContact.pairs=lapply(given_match, function(ll)   tryCatch(ll$threshold, error=function(e) 0)))  

  cat(date(),'finished')
  
  return(ret.list)
  

}

CompareBenchmark <- function(results.l, benchmark.pairs, refIDs=c('refID.1','refID.2')){
  
  benchmark.pairs <- do.call('paste',c(benchmark.pairs[refIDs],sep='.'))
  
  getStats <- function(df, benchmark.pairs){
    if( is.null(df)  ) return(c(cp=0, fp=0, cn=total.pairs-length(benchmark.pairs),fn=length(benchmark.pairs)))
    df.pairs <- do.call('paste',c(df[refIDs],sep='.'))
    cp <-  sum(df.pairs %in% benchmark.pairs)
    fp <- sum(!(df.pairs %in% benchmark.pairs))
    fn <- sum(!(benchmark.pairs %in% df.pairs))
    negatives <- total.pairs-length(df.pairs) #linked pairs-length(df.pairs)
    return(c(cp=cp, fp=fp, cn=negatives-fn,fn=fn))
  }
  
  ret.list <- list()
  for(str.cutoff in c(0, 0.74, .76,.78,.80)){
    cat(date(),str.cutoff,'current cutoff')
    ret.list[[paste('postfp',str.cutoff,sep='')]] <- data.frame(t(data.frame(lapply(1:length(results.l$dedupContact.pairs),function(ind){
      df <- results.l$dedupContact.pairs[[ind]]
      FP <- AssessLoss(df, compThreshold = str.cutoff,expect.nMatch = 1)$FP
      getStats(df[!FP,],benchmark.pairs)
    }))))
    rownames(ret.list[[paste('postfp',str.cutoff,sep='')]])<- names(results.l$dedupContact.pairs)
    
    for(age.cutoff in c(10,15)){
      cat(date(),age.cutoff,'current cutoff')
      ret.list[[paste('aged',age.cutoff,'.postfp',str.cutoff,sep='')]] <- data.frame(t(data.frame(lapply(1:length(results.l$dedupContact.pairs),function(ind){
        df <- results.l$dedupContact.pairs[[ind]]
        FP <- AssessLoss(df, compThreshold = str.cutoff,expect.nMatch = 1)$FP
        aged <- !is.na(df$age.2) & abs(df$age.1-df$age.2) <=age.cutoff
        getStats(df[!FP & aged,],benchmark.pairs)
      }))))
      rownames(ret.list[[paste('aged',age.cutoff,'.postfp',str.cutoff,sep='')]]) <- names(results.l$dedupContact.pairs)
      if(age.cutoff==15){
        ret.list[[paste('aged',age.cutoff,'.postfp',str.cutoff,'villageRestric',sep='')]] <- data.frame(t(data.frame(lapply(1:length(results.l$dedupContact.pairs),function(ind){
          df <- results.l$dedupContact.pairs[[ind]]
          FP <- AssessLoss(df, compThreshold = str.cutoff,expect.nMatch = 1)$FP
          aged <- !is.na(df$age.2) & abs(df$age.1-df$age.2) <=age.cutoff
          fail.villages <- df$village.1!= df$village.2 & !is.na(df$age.2) & abs(df$age.1-df$age.2) >10 
          getStats(df[!FP & aged & !fail.villages,],benchmark.pairs)
        }))))
        rownames(ret.list[[paste('aged',age.cutoff,'.postfp',str.cutoff,'villageRestric',sep='')]])<- names(results.l$dedupContact.pairs)
      }
    }
  }
  return(ret.list)
}

GetPercents <- function(benchmark.l){
  cat(' calculating percentage')
  lapply(benchmark.l,function(df){
    ret.df <- cbind(df[c('cp','fn')]/golden.p,df[c('cn','fp')]/golden.n)
    ret.df$lr.pos <- ret.df$cp/ret.df$fp
    ret.df$cpTofp <- df$cp/df$fp
    rownames(ret.df) <- rownames(df)
    return(ret.df)})
}


