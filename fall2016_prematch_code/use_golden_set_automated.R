
clean_golden_set_input_file = commandArgs()[1]
match_l_input_file = commandArgs()[2]
parish = commandArgs()[4]
output_file = commandArgs()[3]


# cluster version of use_golden_set

source('/home/yiqun.chen/goldenset_code/use_golden_set_source.R')

################# this needs to be corrected to be "correct golden pairs" ##########
load(file = paste(clean_golden_set_input_file,parish,'Clean_GoldenSet.RData',sep=''))

#write.csv(golden.set,file='code/goldenSet700.csv',quote=F,row.names=FALSE)
#golden.pairs <- golden.set[golden.set$link==1,c('refID.1','refID.2')]


################ this needs to be corrected to be the match.l file #####
load(file = paste0(match_l_input_file, paste("matches_7000_param",parish, sep = "_"),".Rdata"))


total.pairs <-  nrow(train.link$pairs)  #4996967 ## pairs in train.link
golden.p <- nrow(golden.pairs) #413289
golden.n <- total.pairs-golden.p #4583678


##############################################
### No age bounding after dedup 
##############################################

noAgeBound <- GetResults(combined_match.l)
noAgeBound.results <- CompareBenchmark(noAgeBound,golden.pairs)
noAgeBound.percent <- GetPercents(noAgeBound.results)


threshold_no_age_bound = lapply(combined_match.l,function(ll)ll$threshold)
threshold_no_age_bound[sapply(threshold_no_age_bound, is.null)] <- NA
threshold_no_age_bound = matrix(unlist(threshold_no_age_bound))
used_index_no_age_bound <-  (which(is.na(threshold_no_age_bound)==FALSE))

save(noAgeBound,noAgeBound.results,noAgeBound.percent,threshold_no_age_bound  ,file = paste0(output_path,parish,'use_golden_set.RData'))

do.call('rbind',lapply(noAgeBound.percent,function(method){
  powered <- method$cp >=0.85
  max.lpr <- max(method$lr.pos[powered & method$lr.pos!=Inf])
  method[powered & method$lr.pos==max.lpr,]
}))


power_threshold_result <- do.call('rbind',lapply(noAgeBound.percent,function(method){
  powered <- method$cp >=0.85
  max.lpr <- max(method$lr.pos[powered & method$lr.pos!=Inf])
  method[powered & method$lr.pos==max.lpr,]
}))

write.csv(power_threshold_result, file =paste0(output_path,parish,'power_threshold_result.csv') )



