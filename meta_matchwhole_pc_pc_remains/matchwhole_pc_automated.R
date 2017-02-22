library(Rsolnp)
library(RecordLinkage)

source('~/yiqun_code/CleanNames_methods.R')

##################################################################################

parish <- commandArgs()[3]
type <- commandArgs()[4]
output_path <- commandArgs()[2]
best_algo_param_path <- commandArgs()[1]
num_batches <- as.numeric( commandArgs()[5])
prematch_linkwhole_path <- commandArgs()[6]
matchwhole_output_path <- commandArgs()[7]
# load the best method and corresponding parameter
load(best_algo_param_path)

use.param <-  chosen_algo_param  
use.method <-  chosen_algo
# double check on this 
weight.split.ind <- 7
# the order of attributes same as trainset 
att.order <- c('name1','name2','name3','age','village','sex','name4','othername.prematch','suffix','honorific')
use.vars <- c('name1','name2','name3','age','village','sex','name4','othername.prematch','suffix','honorific','uniqueID','refID','origName')

data1 <- list()
frequencies <- numeric()
# 66 is the number of batches, we can keep track of it back in linkwhole 
# remember to do the whole batches in the end


# change BACK!!!!! (include all files)

#  ls | wc -l the folder 
all.links <- lapply(c(1:num_batches),function(x){
  cat(date(),x,'load\n')
  
  # 
  load(paste(prematch_linkwhole_path,'myPrematch_link_',parish,'_',type,x,'.RData',sep=''))
  
  #browser()
  # start from 1 we need to initialize ; initialize starting from 1
  
  if(x==1){
    data1 <<- prematch.link$data1
  }
  prematch.link$data1 <- NULL
  return(prematch.link)
})


frequencies <- all.links[[1]]$frequencies

# splitting among less important ones
nsplits <- length(frequencies)-(weight.split.ind) + 1


attr.w8 <- c(use.param[-length(use.param)][1:(weight.split.ind-1)], rep(use.param[-length(use.param)][length(use.param)-1]/nsplits,nsplits))

w8.quantile <- use.param[length(use.param)]
error.rate <- GetErrorRates(attr.w8,freq = frequencies,start.df = data.frame(0.01+.01*diag(length(attr.w8))))


save(data1,all.links,w8.quantile,error.rate, use.param,use.method, file=paste(matchwhole_output_path,'all.links.w8.prelim',parish,'_',type,'.RData',sep=''))


cat(date(),'computing weights\n')

# vect are the weghts computed 
weights.vect <- numeric()
all.weights <- lapply(1:length(all.links),function(ind){
  cat(date(),ind, 'computing weights\n')
  linkdata <- all.links[[ind]]
  weight.data <- epiWeights(rpairs=linkdata,e=as.numeric(error.rate[1,(1:length(attr.w8))]))
  # originally commented out 
  weights.vect <<- c(weights.vect,weight.data$Wdata)
  return(weight.data)
})

rm(all.links) 

cat(date(),'computing threshold\n')
threshold <- gpdEst(weights.vect,thresh=quantile(weights.vect,probs=w8.quantile))

cat(date(),'cutting  weights\n')
# recordlinkage
all.weights <- lapply(1:length(all.weights),function(ind){
  cat(date(),ind,'cutting  weights\n')
  linkdata <- all.weights[[ind]]
  linkdata$pairs <- linkdata$pairs[linkdata$Wdata>=threshold,]
  linkdata$Wdata <- linkdata$Wdata[linkdata$Wdata>=threshold]
  return(linkdata)
})


# how to check these are computed correctly>
# sum(trial_5_num * attr.w8) where trial_5_num is a column taken from all. weights' 1 and pairs

cat(date(),'making pass weights\n')
pass.weight <- all.weights[[1]]
data2.n <- numeric()
pass.weight$data2 <- do.call('rbind',lapply(all.weights,function(ll){
  data2.n <<- c(data2.n,nrow(ll$data2))
  return(ll$data2)}))


# keep the frequency types, and runs through all.weights and do rbinding 
pass.weight$pairs <- do.call('rbind',lapply(1:length(all.weights),function(ind){
  ll <- all.weights[[ind]]
  if(ind>1) ll$pairs$id2 <- ll$pairs$id2 + sum(data2.n[1:(ind-1)])
  return(ll$pairs)
}))

pass.weight$Wdata <- unlist(lapply(all.weights,function(ll)ll$Wdata))

pass.weight$data1 <- data1

cat(date(),'saving pass weights\n')

# change to appropriate directory later

save(pass.weight,threshold, use.param,use.method, file=paste(matchwhole_output_path,'myMatches_PassWeights_best_method_',parish,'_',type,'.RData',sep=''))

#######################################
make.ret <- function(df, dedupContact, attOrder=use.vars, contactID,id.vars=c('uniqueID','refID','origName')){
  dup <- rep(FALSE,nrow(df))
  df <- df[df$link==1,]
  if(dedupContact) dup <- duplicated(df[[contactID]])
  return(df[!dup ,c('id1','id2', paste(rep(setdiff(attOrder,id.vars),each=2),rep(1:2,length(setdiff(attOrder,id.vars))),sep='.'),
                    paste('sim',setdiff(attOrder,id.vars),sep='.'),
                    paste(rep(id.vars,each=2),rep(1:2,length(id.vars)),sep='.'),
                    'Weight','link')])
}

############################# adjust if arguments accordingly ######################
if( type=='PC'){
  
  link.pairs <- getLinkedPairs(reclink.df = pass.weight,attOrder = use.vars,threshold = threshold,dedupContact = T,age.bound = NULL,partiID = 'refID.1',contactID = 'refID.2',simple=T)$pre.fp
  
  bad.name <- CheckBadName(link.pairs,compThreshold = .9) #CheckBadName(link.pairs,compThreshold = .9)
  
  # age bound 10 instead of 15; -> back to 15
  within.age <- !is.na(link.pairs$age.2) & ( (abs(link.pairs$age.2-link.pairs$age.1) < 5 & link.pairs$age.1< 15) | (link.pairs$age.1>= 15 & abs(link.pairs$age.2-link.pairs$age.1) <= 15))
  
  link.pairs$link[!within.age | bad.name] <- 0
  #
  # good.village <- link.pairs$village.2!='' & (link.pairs$sim.village >= .8 | 
  #                                               unlist(mclapply(mc.cores=4,1:nrow(link.pairs),function(xx)grepl(link.pairs$village.1[xx],link.pairs$village.2[xx]) | 
  #                                                                 grepl(link.pairs$village.2[xx],link.pairs$village.1[xx]))))
  # sanity check
  # tail(within_age_link.pairs[,c('origName.1','origName.2','age.1','age.2','village.1','village.2')],10)
  #bad.name.hard <- CheckBadName(link.pairs,compThreshold = .95)
  #link.pairs$link[!good.village & bad.name.hard] <- 0
  #link.pairs$link[!good.village & (is.na(link.pairs$age.2) | abs(link.pairs$age.2-link.pairs$age.1) > 5)] <- 0  ### if only one name and mismatch village or name1
  #link.pairs$link[((is.na(link.pairs$age.2) | abs(link.pairs$age.2-link.pairs$age.1) > 10)) & bad.name.hard] <- 0
  
  #link.pairs$link[link.pairs$refID.2=='24106337037-2-14' & link.pairs$refID.1=='24106337034-1'] <- 1
  final.link.pairs <- make.ret(link.pairs,T,contactID='refID.2')
  
  link.pairs <- final.link.pairs
  save(link.pairs,threshold,use.param, use.method, file=paste(matchwhole_output_path ,'myMatches_best_method_Matched_',parish,'_',type,'.RData',sep=''))
}

