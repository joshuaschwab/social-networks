# load the online golden set

##### 
#parish = 'muyembe'
#parish = 'nsiika'
#parish = 'nyatoto'
#parish = 'magunga'
# parish = 'ogongo'
#parish = 'mitooma'
#parish = 'rugazi'
#parish = 'nsiinze'
#parish = 'nankoma'
#parish = 'kisegi'
#parish = 'kitwe'
#parish = 'rubaare'
#parish = 'nyamrisra'
parish = 'kitare'
golden_set_file = paste0('/home/yiqun.chen/trainset_data/',parish,'_goldenSet1100_corrected.csv')
# load the modified csv

golden_set <- read.csv2(golden_set_file, sep =",")

golden_set_na_removed <- golden_set[!is.na(golden_set$link),]

prematch_trainset_file = paste0('/home/yiqun.chen/trainset_data/UseSASprematch_trainSet_',parish,'.RData')
load(prematch_trainset_file)

golden.pairs <- golden_set_na_removed[golden_set_na_removed$link==1,c('refID.1','refID.2')]


save(golden.pairs, train.link,  file = paste0('/home/yiqun.chen/goldenset_data/',parish,'Clean_GoldenSet.RData'))


