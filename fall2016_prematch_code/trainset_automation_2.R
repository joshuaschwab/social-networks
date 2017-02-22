#################################################################################
#################################################################################
#################################################################################
### make golden set using an algorithm just as aid

# QUESTION: it does not work here since we have 10 weights now ... instead of 9, will just spare
# 0.01 from the 0.45 part..
# weight.train <- GetWeightedData(attr.w8=c(0.45, 0.13, 0.18, 0.14, 0.06 ,rep(0.04/4,4)), train.link)


weight.train <- GetWeightedData(attr.w8=c(0.44, 0.13, 0.18, 0.14, 0.06 ,rep(0.05/5,5)), train.link)

golden.set <- getLinkedPairs(weight.train$weight.data,attOrder = use.vars,age.bound = NULL,weighted = T,threshold = 0,partiID = 'refID.1',contactID='refID.2')$dupContact


columns.to.select.1 <- c(paste(rep(c('name1','name2','name3','age','village'),each=2),rep(1:2,5),sep='.'),'link','Weight','refID.2','refID.1')
columns.to.select.2 <-  paste(rep(c('origName','name4','suffix','honorific','uniqueID'),each=2),rep(1:2,6),sep='.')
columns.to.select.3 <- c(paste('sim',c('name1','name2','name3','age','name4','suffix','honorific','village'),sep='.'),'id1','id2')
columns.to.select <- c(columns.to.select.1,columns.to.select.2,columns.to.select.3)

golden.set.partial <- golden.set[columns.to.select]

#> setdiff(columns.to.select,names(golden.set))
#[1] "othername.1"   "othername.2"   "sim.othername", we  do not have these in golden set...
# tentative plan, remove then, QUESTION

has.ages <- (!is.na(golden.set.partial$age.1) & !is.na(golden.set.partial$age.2) )
cat(date(),sum(has.ages==1),'number of pairs that has age' ) # 385460 has age 2! only half of them?!

golden.set.partial$link[has.ages & abs(golden.set.partial$age.1 -golden.set.partial$age.2)>10] <- 0     


new.order <- order(golden.set.partial$refID.2, golden.set.partial$Weight,decreasing = T)
golden.set.partial <- golden.set.partial[order(golden.set.partial$refID.2, golden.set.partial$Weight,decreasing = T),]

dup <- duplicated(golden.set.partial$refID.2)
golden.set.partial$link[dup] <- 0

######### threshold ######### 

golden.set.partial$link[golden.set.partial$Weight < .75] <- 0

length(which(golden.set.partial$link==1))
# 319 matched
### manually correct these links


write.csv(golden.set.partial,file=paste(output_path,parish,'_goldenSet1100.csv',sep=''),quote=F,row.names=FALSE)
