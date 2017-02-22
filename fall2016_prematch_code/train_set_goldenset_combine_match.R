
parish = commandArgs()[3]
output_path = commandArgs()[2]
input_path = commandArgs()[1]

# set batch_number
batch_number = 1:14
# 
combined_match.l <- list()




######################### combining batches  ######################### 

for (i in batch_number) {
  file = paste0(input_path, paste(parish,'run_index',i,'match.l.RData',sep='_'))
  load(file)
  combined_match.l <- append(combined_match.l,match.l)
  rm(match.l)
}


save(combined_match.l,file = paste0(output_path, paste("matches_7000_param",parish, sep = "_"),".Rdata"))


# 85 powers for power and controlling power, we need to the lowest ratio



