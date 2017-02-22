#### use golden set to train algorithms
### runs algorithm on trainingset
# 
parish = commandArgs()[3]
output_path = commandArgs()[2]
input_path = commandArgs()[1]
parameter_input = commandArgs()[4]
train_set_data_path = commandArgs()[5]


#### specifies which batch is being run 
run.index = 7
cat(date(),run.index,'match.l being created')

library(RecordLinkage)
library(Rsolnp)
library(hitandrun)
library(doParallel)
source('~/yiqun_code/CleanNames_methods.R')


####################### we only need train link data, which can be obtained from
# golden set or from trainset run data (latter preferred) #################

train_set_file=paste(train_set_data_path,'UseSASprematch_trainSet_',parish,'.RData',sep='')

load(file = train_set_file)

ncl <- 12
RUNPARALLEL <- T # true for cluster

load(file = parameter_input)

###################### do the batch size loop ###################### 

batch_size = 100
row_index_start  = seq( 1,nrow(param),by=batch_size)
row_index_end = row_index_start+batch_size-1

cat(date(),"run.index:", run.index, 'We started processing subparameters')

for (counter in seq(1,length(row_index_start), by =1)){
  
  starting_index = row_index_start[counter]
  ending_index = min(row_index_end[counter],nrow(param))
  cat('currently doing ',starting_index,' to ',starting_index )
  sub_param_space = param[starting_index:ending_index,]
  
  match.l <- OptimMatchParam(link.data = train.link, weight.split.ind = 7 ,param.space = sub_param_space ,strThreshold=0.74)
  
  
  save(match.l,file = paste0(output_path, paste(parish,'run_index',counter,'match.l.RData',sep='_')))
  
  
}



















