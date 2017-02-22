
library(Rsolnp)
library(RecordLinkage)
library(parallel)
source('~/yiqun_code/CleanNames_methods.R')

threshold_for_weight =  as.numeric(commandArgs()[1])
threshold =  as.numeric(commandArgs()[1])
total_num_of_batches = as.numeric(commandArgs()[2])
chunk_len = as.numeric(commandArgs()[3])

type <- commandArgs()[4]
parish <- commandArgs()[5]

best_param_path <- commandArgs()[6]

load(best_param_path)

input_path = commandArgs()[7]
output_path = commandArgs()[8]

#################

use.param <-  chosen_algo_param
use.method <-  chosen_algo
# double check on this 
weight.split.ind <- 7
# the order of attributes same as trainset 
att.order <- c('name1','name2','name3','age','village','sex','name4','othername.prematch','suffix','honorific')
use.vars <- c('name1','name2','name3','age','village','sex','name4','othername.prematch','suffix','honorific','uniqueID','refID','origName')


start_indices <- seq(1,total_num_of_batches,by=chunk_len)




############# we load the data one by one and 

thrsholded_all_weights_collection <- vector()

thresholding_weights <- function(indices, input_directory, threshold_value){
  
  
  start_index = indices[1]
  end_index = indices[length(indices)]
  
  cat(date(),'loading  weights\n')
  
  load(file=paste(input_path, 'all.weights', '_',start_index,'_',end_index,'_', parish,'_',type,'.RData',sep='') )
  
  cat(date(),'cutting  weights\n')
  
  current.all.weights <- lapply(1:length(current.all.weights),function(ind){
    cat(date(),ind,'cutting  weights\n')
    linkdata <- current.all.weights[[ind]]
    linkdata$pairs <- linkdata$pairs[linkdata$Wdata>=threshold_value,]
    linkdata$Wdata <- linkdata$Wdata[linkdata$Wdata>=threshold_value]
    return(linkdata)
  })
  
  
  
  return(current.all.weights)
  
  
}


for (i in 1:length(start_indices)){
  
  
  
  cat(date(), 'currently doing', start_indices[i],'to' ,min(total_num_of_batches,(start_indices[i]+chunk_len-1)))
  current_indices = start_indices[i]:min(total_num_of_batches, (start_indices[i]+chunk_len-1))
  
  current_thresholded_weights = thresholding_weights(current_indices, input_path, threshold_for_weight)
  
  thrsholded_all_weights_collection <- c(thrsholded_all_weights_collection, current_thresholded_weights)
}



# how to check these are computed correctly>
# sum(trial_5_num * attr.w8) where trial_5_num is a column taken from all. weights' 1 and pairs

cat(date(),'making pass weights\n')
pass.weight <- thrsholded_all_weights_collection[[1]]
data2.n <- numeric()
pass.weight$data2 <- do.call('rbind',lapply(thrsholded_all_weights_collection,function(ll){
  data2.n <<- c(data2.n,nrow(ll$data2))
  return(ll$data2)}))


# keep the frequency types, and runs through all.weights and do rbinding 
pass.weight$pairs <- do.call('rbind',lapply(1:length(thrsholded_all_weights_collection),function(ind){
  ll <- thrsholded_all_weights_collection[[ind]]
  if(ind>1) ll$pairs$id2 <- ll$pairs$id2 + sum(data2.n[1:(ind-1)])
  return(ll$pairs)
}))

pass.weight$Wdata <- unlist(lapply(thrsholded_all_weights_collection,function(ll)ll$Wdata))


data_1_name = as.character(min(total_num_of_batches,50))
data1_file = paste(output_path,'all.links.w8.prelim','_','1','_',data_1_name,'_',parish,'_',type,'.RData',sep='')

load(file = data1_file)

pass.weight$data1 <- data1

cat(date(),'saving pass weights\n')

# change to appropriate directory later

save(pass.weight,threshold, use.param,use.method, file=paste(output_path,'pc_remains_myMatches_PassWeights_best_method_',parish,'_',type,'.RData',sep=''))

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

############################# adjust accordingly ######################
if( type=='PC'){
  
  link.pairs <- getLinkedPairs(reclink.df = pass.weight,attOrder = use.vars,threshold = threshold,dedupContact = T,age.bound = NULL,partiID = 'refID.1',contactID = 'refID.2',simple=T)$pre.fp
  
  bad.name <- CheckBadName(link.pairs,compThreshold = .9) #CheckBadName(link.pairs,compThreshold = .9)
  
  # age bound 10 instead of 15; -> back to 15
  within.age <- !is.na(link.pairs$age.2) & ( (abs(link.pairs$age.2-link.pairs$age.1) < 5 & link.pairs$age.1< 15) | (link.pairs$age.1>= 15 & abs(link.pairs$age.2-link.pairs$age.1) <= 15))
  
  link.pairs$link[!within.age | bad.name] <- 0
  #
  good.village <- link.pairs$village.2!='' & (link.pairs$sim.village >= .8 | 
                                                unlist(mclapply(mc.cores=4,1:nrow(link.pairs),function(xx)grepl(link.pairs$village.1[xx],link.pairs$village.2[xx]) | 
                                                                  grepl(link.pairs$village.2[xx],link.pairs$village.1[xx]))))
  # sanity check
  # tail(within_age_link.pairs[,c('origName.1','origName.2','age.1','age.2','village.1','village.2')],10)
  bad.name.hard <- CheckBadName(link.pairs,compThreshold = .95)
  link.pairs$link[!good.village & bad.name.hard] <- 0
  #link.pairs$link[!good.village & (is.na(link.pairs$age.2) | abs(link.pairs$age.2-link.pairs$age.1) > 5)] <- 0  ### if only one name and mismatch village or name1
  link.pairs$link[((is.na(link.pairs$age.2) | abs(link.pairs$age.2-link.pairs$age.1) > 10)) & bad.name.hard] <- 0
  

  final.link.pairs <- make.ret(link.pairs,T,contactID='refID.2')
  
  link.pairs <- final.link.pairs
  save(link.pairs,threshold,use.param, use.method, file=paste(output_path,'pc_remains_myMatches_best_method_Matched_',parish,'_',type,'.RData',sep=''))
  
}
