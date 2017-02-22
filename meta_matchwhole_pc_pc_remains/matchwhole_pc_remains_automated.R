library(Rsolnp)
library(RecordLinkage)

source('~/yiqun_code/CleanNames_methods.R')

##################################################################################

# we can probably pre-specify this by sourcing the code and passing in arguments
#################
total_num_of_batches = as.numeric(commandArgs()[1])
chunk_len = as.numeric(commandArgs()[2])
type <- commandArgs()[3]
parish <- commandArgs()[4]
best_algo_param_path <- commandArgs()[5]
input_path <- commandArgs()[6]
output_path <- commandArgs()[7]


load(file = best_algo_param_path)

#################

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
# ls *.RData  | wc -l

start_indices <- seq(1,total_num_of_batches,by=50)
threshold_collection <- rep(0,length(start_indices))

compute_weight_and_threshold <- function(indices, input_directory){
  
  start_index = indices[1]
  end_index = indices[length(indices)]
  
  
  current.all.link <- lapply(indices, function(x){
    
    cat(date(),x,'load\n')
    
    
    # 
    current_file <- paste(pc_remains_input,'myPrematch_link_',parish,'_',type,x,'.RData',sep='')
    load(current_file)
    #browser()
    # start from 1 we need to initialize ; initialize starting from 1
    
    if(x==1){
      data1 <<- prematch.link$data1
    }
    prematch.link$data1 <- NULL
    return(prematch.link)
  })
  
  
  frequencies <- current.all.link[[1]]$frequencies
  # splitting among less important ones
  nsplits <- length(frequencies)-(weight.split.ind) + 1
  
  
  attr.w8 <- c(use.param[-length(use.param)][1:(weight.split.ind-1)], rep(use.param[-length(use.param)][length(use.param)-1]/nsplits,nsplits))
  w8.quantile <- use.param[length(use.param)]
  error.rate <- GetErrorRates(attr.w8,freq = frequencies,start.df = data.frame(0.01+.01*diag(length(attr.w8))))
  
  output_file=paste(output_path,'all.links.w8.prelim','_',start_index,'_',end_index,'_',parish,'_',type,'.RData',sep='')
  
  save(frequencies, attr.w8,error.rate, data1,w8.quantile,error.rate, use.param,use.method, file = output_file)
  
  save(current.all.link, file=paste(output_path,'all.links','_',start_index,'_',end_index,'_',parish,'_',type,'.RData',sep=''))
  
  cat(date(),'computing weights\n')
  
  # vect are the weghts computed 
  weights.vect <- numeric()
  
  current.all.weights <- lapply(1:length(current.all.link),function(ind){
    cat(date(),ind, 'computing weights\n')
    linkdata <- current.all.link[[ind]]
    weight.data <- epiWeights(rpairs=linkdata,e=as.numeric(error.rate[1,(1:length(attr.w8))]))
    # originally commented out 
    weights.vect <<- c(weights.vect,weight.data$Wdata)
    return(weight.data)
  })
  
  rm(current.all.link) 
  
  
  save(current.all.weights, file=paste(output_path, 'all.weights', '_',start_index,'_',end_index,'_', parish,'_',type,'.RData',sep='') )
  
  rm(current.all.weights)
  
  cat(date(),'computing threshold\n')
  threshold <- gpdEst(weights.vect,thresh=quantile(weights.vect,probs=w8.quantile))
  
  cat(date(),'computed threshold', threshold)
  
  return(threshold)
  
}


for (i in 1:length(start_indices)){
  
  cat(date(), 'currently doing', start_indices[i],'to' ,min(total_num_of_batches,(start_indices[i]+chunk_len-1)))
  current_indices = start_indices[i]:min(total_num_of_batches, (start_indices[i]+chunk_len-1))
  current_threshold = compute_weight_and_threshold(current_indices, input_path)
  
  threshold_collection[i] = current_threshold
}



save(threshold_collection, file = paste0(output_path,'threshold_collection_',parish,'_chunk_len_',chunk_len,'.RData'))



