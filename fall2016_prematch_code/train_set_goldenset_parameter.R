#### use golden set to train algorithms
### runs algorithm on trainingset
# 


parish = commandArgs()[3]
output_path = commandArgs()[2]
input_path = commandArgs()[1]
param.n <- commandArgs()[4]
param.n <- as.numeric(param.n)
cat(date(),param.n)

run.index = 7

cat(date(),run.index,'parameters are being created')


library(RecordLinkage)
library(Rsolnp)
library(hitandrun)
library(doParallel)
source('~/yiqun_code/CleanNames_methods.R')

ncl <- 12
RUNPARALLEL <- T # true for cluster
# 7*500 =3500; 1000*7 = 7000
#################################################################################
## make param space

# Use golden sets to find weights for the attributes for the whole dataset
# What is the best combination of weights based on the scores we get

set.seed(12345678)
param <- MakeParamSpace(param.n,weight.len = 7 ,c(.92,.98))

# try param works

final_num = param.n*7
cat(date(),final_num)

save(param,file = paste(output_path,parish,'_param_', final_num,'.RData',sep=''))



