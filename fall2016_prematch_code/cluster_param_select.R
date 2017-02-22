GetResults <- function(match.l){
  
  
  ret.list <- list(
    threshold=sapply(match.l,function(ll)ll$threshold),
    dedupContact.pairs=lapply(match.l, function(ll)ll$linked.pairs.dedupContact))
  
  return(ret.list)
  
  
}

CompareBenchmark <- function(results.l, benchmark.pairs, refIDs=c('refID.1','refID.2')){
  
  AssessLoss <- function(linked.pairs,compThreshold,components=c('sim.name1','sim.name2','sim.name3'), expect.nMatch){
    if(length(compThreshold)==1){compThreshold <- rep(compThreshold, length(components))}
    f.p <- rowSums(data.frame(lapply(1:length(components),function(ind){
      comp <- linked.pairs[components[ind]]
      as.numeric(!is.na(comp) & comp< compThreshold[ind])
    }))) > 0  ## any component < threhsold
    ret.list <- list(FPrate=sum(f.p)/nrow(linked.pairs),matched=(nrow(linked.pairs)-sum(f.p))/expect.nMatch,FP=f.p)
    ret.list$cost <- ret.list$FPrate/ret.list$matched
    return(ret.list)
  }
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
    cat(date(),str.cutoff,'current cutoff','\n')
    ret.list[[paste('postfp',str.cutoff,sep='')]] <- data.frame(t(data.frame(lapply(1:length(results.l$dedupContact.pairs),function(ind){
      df <- results.l$dedupContact.pairs[[ind]]
      FP <- AssessLoss(df, compThreshold = str.cutoff,expect.nMatch = 1)$FP
      getStats(df[!FP,],benchmark.pairs)
    }))))
    rownames(ret.list[[paste('postfp',str.cutoff,sep='')]])<- names(results.l$dedupContact.pairs)
    
    for(age.cutoff in c(10,15)){
      cat(date(),age.cutoff,'current cutoff','\n')
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







parish = 'nsiika'#'kisegi'#'nyamrisra'#'rubaare' #'kitwe' #'ogongo'
#  load matches

cat('currently on', parish, '\n')

#  load(file =paste0('/home/yiqun.chen/goldenset_outputs/matches_7000_param_',parish,'.Rdata'))

# load golden pairs
load(file=paste0('/home/yiqun.chen/goldenset_data/',parish,'Clean_GoldenSet.RData'))

#noAgeBound <- GetResults(combined_match.l)
total.pairs <-  nrow(train.link$pairs)  #4996967 ## pairs in train.link
golden.p <- nrow(golden.pairs) #413289
golden.n <- total.pairs-golden.p #4583678
cat(total.pairs,golden.p,golden.n,'pairs stats','\n')

#noAgeBound <- GetResults(combined_match.l)
load(file =paste0('/home/yiqun.chen/goldenset_outputs/matches_7000_param_',parish,'.Rdata'))

noAgeBound <- GetResults(combined_match.l)

noAgeBound.results <- CompareBenchmark(noAgeBound,golden.pairs)

noAgeBound.percent <- GetPercents(noAgeBound.results)
save(noAgeBound.percent, file = paste0('/home/yiqun.chen/goldenset_param_select/',parish, '_no_age_bound_percent.RData'))





