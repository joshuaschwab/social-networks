source('/home/yiqun.chen/yiqun_code/CleanNames_methods.R')
source('/home/yiqun.chen/yiqun_code/myCompareRL.r')
library(RecordLinkage)
library(Rsolnp)


parish = commandArgs()[3]
output_path = commandArgs()[2]
input_path = commandArgs()[1]

load(paste(input_path,'myPrematch_',parish,'.RData',sep=''))


param.len <- 11

#(5 weights for 4 + (5) attr, 1 paretoquantile) # 7+1 (sex)+ line 36 names
villages.study <- sort(unique(prematch.p$village))
#####################################################################
### use a sample SEARCH-village contacts
set.seed(1234)
train.sample <- randomRows(prematch.c[prematch.c$village %in% prematch.p$village,], 100*param.len)  ## 1100 samples that are in fact participants

# recover the original name
train.sample$origName <- do.call('paste',c(train.sample[c('name1','name2','name3','name4')],sep=' '))

# get the reference id
train.sample$refID <- train.sample$contactID

#### re-extract same train.sample from new prematch.c

orig.trainsample <- train.sample
train.id <- train.sample$contactID
# TRIAL
train.id.yc.trial <- train.sample$SEARCHID
# get the social network type as well
c.snatype <- gsub( "\\..*$", "", gsub('^.*free\\.','sna',prematch.c$snTypes))
table(c.snatype)
prematch.c$sna <- c.snatype


prematch.c$train.id2 <- paste(prematch.c$SEARCHID,prematch.c$sna,sep='_')

# Question! 
# a typical element of prematch.c$train.id2 looks like "34103508008-1_emot" with
# emot replacble by sna1/2/3/4/5/6
# But a typical element of train.id looks like "34202519082-2-12"
# How to correct? or what does each field mean ...
# tracing back tells us that for the train.id part, 12 refers to the time that
# the previous id appears and _emot refers to the sntype... 

# QUESTION: are these "suffix" really needed for us to match the id's? What if we just
# mathc the first 11+1 search id? 


# A record has different lines(permutated names) due to permutation
# 
train.id.use <- unique(c(intersect(train.id,prematch.c$train.id2),'24104320056-2_sna1','24204300060-1_sna2'))

train.id.use.yc.trial <- unique(c(intersect(train.id.yc.trial,prematch.c$SEARCHID),'24104320056-2','24204300060-1'))

# prematch.c$SEARCHID

# collection of all the lines
train.sample <- prematch.c[match(train.id.use,prematch.c$train.id2),]

train.sample.trial <- prematch.c[match(train.id.use.yc.trial,prematch.c$SEARCHID),]

## expand pernamutations of sample.c

# this step!
train.sample2 <- GetNamePermutations(train.sample,ref.id='contactID') 

train.sample2.trial <- GetNamePermutations(train.sample.trial,ref.id='contactID') 



length(unique(train.sample2$contactID)) ## 3631

length(unique(train.sample2.trial$contactID)) ## 895

#################################################################################
###  blcoking on different variables, produce pairs for train.sample: left=permuated, right=non-permuated
# we should keep use.vars the same throughout matchwhole and linkwhole
use.vars <- c('name1','name2','name3','age','village','sex','name4','othername.prematch','suffix','honorific','uniqueID','refID','origName')

train.link <- GetMultiBlockLinkage(prematch.p2[use.vars],train.sample2.trial[use.vars],blockflds=c('village','name1','name2','name3'),  strcmp=c('name1','name2','name3','name4','othername.prematch','suffix','honorific','village'), intcmp=c('age','sex'),exclude=c('origName','refID','uniqueID'))


rm(prematch.c2)
#rm(prematch.search.c2)

save(prematch.p, prematch.p2, train.sample, train.sample2.trial,orig.trainsample, train.id.use.yc.trial, train.link, use.vars,  file=paste(paste0(output_path, 'UseSASprematch_trainSet_'),parish,'.RData',sep=''))

