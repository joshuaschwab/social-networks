library(parallel)
source('~/Desktop/YC_code/CleanNames_methods.R')


#####
## use this to clean up linked pairs  in combined pairs.
fixing.new.contacts <- T
################################################################
make.Magunga <- function(){
  community <- 'bugamba'
  parish <- 'bugamba'
  load(paste0('~/Dropbox/Network/10_10_final_pairs/permutation_fixed/combined_final_pairs_bugamba.RData')) ### new participant data based on SNABL data. 
  
  combined_linked_pairs.yc <- combined_linked_pairs
  combined_linked_pairs <- combined_linked_pairs[order(combined_linked_pairs$Weight,decreasing = T),]
  good.name <- !CheckBadName(combined_linked_pairs,compThreshold = .9) & combined_linked_pairs$name2.2!='' & combined_linked_pairs$name2.1!='' & combined_linked_pairs$name2.2!=''  ## all have only one first name for contact
  verygood.name <- !CheckBadName(combined_linked_pairs,compThreshold = .95) &combined_linked_pairs$name2.1!='' &  combined_linked_pairs$name2.2!=''  ## all have only one first name for contact
  within.age <- !is.na(combined_linked_pairs$age.2) & ( (abs(combined_linked_pairs$age.2-combined_linked_pairs$age.1) < 5 & combined_linked_pairs$age.1< 15) 
                                                        | (combined_linked_pairs$age.1>= 15 & abs(combined_linked_pairs$age.2-combined_linked_pairs$age.1) <= 10))
  within.age.5 <- !is.na(combined_linked_pairs$age.2) & ( (abs(combined_linked_pairs$age.2-combined_linked_pairs$age.1) < 5 & combined_linked_pairs$age.1< 15) 
                                                          | (combined_linked_pairs$age.1>= 15 & abs(combined_linked_pairs$age.2-combined_linked_pairs$age.1) <= 5))
  good.village <- combined_linked_pairs$village.2!='' & (combined_linked_pairs$sim.village >= .8 | 
                                                           unlist(mclapply(mc.cores=4,1:nrow(combined_linked_pairs),function(xx)grepl(combined_linked_pairs$village.1[xx],combined_linked_pairs$village.2[xx]) | 
                                                                             grepl(combined_linked_pairs$village.2[xx],combined_linked_pairs$village.1[xx]))))
  verygood.village <- combined_linked_pairs$village.2!='' & (combined_linked_pairs$sim.village >= .9 | 
                                                               unlist(mclapply(mc.cores=4,1:nrow(combined_linked_pairs),function(xx)grepl(combined_linked_pairs$village.1[xx],combined_linked_pairs$village.2[xx]) | 
                                                                                 grepl(combined_linked_pairs$village.2[xx],combined_linked_pairs$village.1[xx]))))
  reject <- ((!good.name & !verygood.village) | (!good.name & !within.age.5) | 
               (!within.age & !verygood.name) | ( !within.age & !verygood.village) | 
               (combined_linked_pairs$name2.2=='' & combined_linked_pairs$name1.2 %in% c('MOTHER','FATHER','SON','DAUGHTER')))
  combined_linked_pairs$link[reject] <- 0
  combined_linked_pairs.fixed <- subset(combined_linked_pairs,link==1)
  save(combined_linked_pairs.fixed,parish, file = '~/Dropbox/Network/10_10_final_pairs/final_fixed_wz_protocol/fixed_final_pairs_bugamba.RData')
  
}