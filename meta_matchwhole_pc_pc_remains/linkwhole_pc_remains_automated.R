library(stringdist)
library(RecordLinkage)
library(Rsolnp)
library(gtools)

source('~/yiqun_code/CleanNames_methods.R')
source('~/yiqun_code/myCompareRL.r')

#################################### linking ######################################## 

# double check whether sex is int comp or str

link.whole <- function(type=c('PC','CC','PP'), data1,data2=NULL, batch.size, blockflds=c('village','name1','name2','name3','sex'),
                       strcmp=c('name1','name2','name3','name4','othername.prematch','suffix','honorific','village'),
                       intcmp=c('age','sex'),exclude=c('origName','refID','uniqueID') ){
  
  if(is.null(data2)){
    frequencies <- apply(data1[att.order],2,function(x) 1/length(unique(x)))
    data2 <- data1
  }
  else{
    frequencies <- apply(rbind(data1[att.order],data2[att.order]),2,function(x) 1/length(unique(x)))
  }
  
  nc2 <- nrow(data2)
  endpoints <- round(seq(1,nc2,by=batch.size))
  batches <- lapply(1:(length(endpoints)-1),function(xx){
    if(xx!=(length(endpoints)-1)){
      return(c(endpoints[xx],endpoints[xx+1]-1))
    }
    else
      return(c(endpoints[xx],nc2))
  })
  last <- batches[[length(batches)]]
  if(diff(last) > batch.size){
    batches[[length(batches)]][2] <- batches[[length(batches)]][1] + batch.size
    batches[[length(batches)+1]] <- c(batches[[length(batches)]][2]+1, nc2)
  }
  print(batches)
  num_of_batches = length(batches)
  save(num_of_batches, file = paste(paste0(output_path,'num_of_batches'),parish,'_',type,'.RData',sep=''))
  # ind is the batch #
  for(ind in 1:length(batches)){
    cat(date(),ind,'batch\n')
    r <- batches[[ind]]
    prematch.link <- GetMultiBlockLinkage(data1[use.vars],data2[r[1]:r[2],use.vars],
                                          blockflds=blockflds,strcmp=strcmp,intcmp=intcmp,exclude=exclude)
    prematch.link$frequencies <- frequencies
    save(prematch.link,frequencies,use.vars, att.order, file=paste(paste0(output_path,'myPrematch_link_'),parish,'_',type,ind,'.RData',sep=''))
  }
}

# need to add sex attributes 
use.vars <- c('name1','name2','name3','age','village','sex','name4','othername.prematch','suffix','honorific','uniqueID','refID','origName')
att.order <- c('name1','name2','name3','age','village','sex','name4','othername.prematch','suffix','honorific')


###################################################

# make sure the code still runs through with smaller trial dataset// remember
# to chanbge batch size accordingly


type  <- commandArgs()[5]
linked_pairs_input <- commandArgs()[4]
parish <- commandArgs()[3]
output_path <- commandArgs()[2]
prematch_input_path <- commandArgs()[1]


load(file = prematch_input_path)
load(file = linked_pairs_input)


## link without blocking for unmatched contacts


unmatch.c2 <- prematch.c2[!(prematch.c2$contactID %in% link.pairs$refID.2[link.pairs$link==1]),]

link.whole('PC',data1=prematch.p2,data2=unmatch.c2,batch.size=50,blockflds=FALSE)


