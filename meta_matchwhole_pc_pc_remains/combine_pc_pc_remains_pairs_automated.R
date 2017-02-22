
### automate this
### quality control" 

# tail(combined_linked_pairs[c('name1.1','name1.2', 'name2.1','name2.2','village.1','village.2')],10)

parish <-  commandArgs()[4]
type  <-  commandArgs()[3]
pc_pairs_path <- commandArgs()[1]
pc_remains_pairs_path <- commandArgs()[2]
output_path <- commandArgs()[5]


load(file = pc_pairs_path)

pc_final_linked_pairs <- link.pairs

load( file=pc_remains_pairs_path)

pc_remains_final_linked_pairs <- link.pairs

combined_linked_pairs <- rbind(pc_final_linked_pairs,pc_remains_final_linked_pairs)

save(parish,pc_final_linked_pairs,pc_remains_final_linked_pairs,combined_linked_pairs, file=paste(output_path,'combined_final_pairs_',parish,'.RData',sep=''))
