################ locations and parameters to specify ##########################

parish = 'bugamba' # fill in parish accordingly 
type_of_match = 'PC'
type = type_of_match
train_set_data_path = '/home/yiqun.chen/trainset_data/'
output_path = ''
prematch_link_data_path = paste0('/home/yiqun.chen/linkwhole_data/linkwhole_',parish,'/')
best_algo_param_path = paste0('/home/yiqun.chen/best_algo_param/best_algo_param_',parish,'.RData') ####### TBA

matchwhole_output_path = paste0('/home/yiqun.chen/matchwhole_data/matchwhole_',parish,'/')

#  ls *.RData | wc -l the linkwhole folder  to get num of batches
num_batches = 269
#############################################################################
###  This part calls matchwhole pc based on the best param and method  ######
#############################################################################

commandArgs <- function() c(best_algo_param_path, output_path,parish,type_of_match, num_batches, prematch_link_data_path,matchwhole_output_path  )

source('/home/yiqun.chen/matchwhole_pc_and_pc_remains_code/matchwhole_pc_automated.R')


#############################################################################
########  This part use the linked pairs to generate pc remains data  #######
#############################################################################


linked_pairs_input <- paste(matchwhole_output_path ,'myMatches_best_method_Matched_',parish,'_',type,'.RData',sep='')

prematch_input_path <- paste('/home/yiqun.chen/prematch_dataset/myPrematch_',parish,'.RData',sep='')
output_path <- paste0('/home/yiqun.chen/linkwhole_pc_remains_data/linkwhole_pc_remains_',parish,'/')

commandArgs <- function() c(prematch_input_path, output_path,parish,linked_pairs_input,type_of_match)

source('/home/yiqun.chen/matchwhole_pc_and_pc_remains_code/linkwhole_pc_remains_automated.R')

#############################################################################
#############  This part matches the generated pc remains data  #############
#############################################################################
load(file = paste(paste0(output_path,'num_of_batches'),parish,'_',type,'.RData',sep=''))

#  ls *.RData | wc -l the linkwhole pc remains folder  to get num of batches
total_num_of_batches <- num_of_batches

chunk_len <- 50

linked_pairs_input <- paste(matchwhole_output_path ,'myMatches_best_method_Matched_',parish,'_',type,'.RData',sep='')

pc_remains_input <- paste0('/home/yiqun.chen/linkwhole_pc_remains_data/linkwhole_pc_remains_',parish,'/')

prematch_input_path <- paste('/home/yiqun.chen/prematch_dataset/myPrematch_',parish,'.RData',sep='')


output_path <- paste0('/home/yiqun.chen/matchwhole_pc_remains_data/matchwhole_pc_remains_',parish,'/')

commandArgs <- function() c(total_num_of_batches, chunk_len,type_of_match, parish, best_algo_param_path, pc_remains_input, output_path)

source('/home/yiqun.chen/matchwhole_pc_and_pc_remains_code/matchwhole_pc_remains_automated.R')

################## by default we use mean of those thresholds, but can always
# come back and change if needed ################## 
load(file = paste0(output_path,'threshold_collection_',parish,'_chunk_len_',chunk_len,'.RData'))

cat('collection of thresholds is ' ,threshold_collection)
cat('mean is ', mean(threshold_collection))
cat('median is ', median(threshold_collection))

#############################################################################
#############  Now we have the threshold hold, we go further to cut  #############
#############################################################################

# we get the threshold by taking mean/median of our threshold statistic,
# we use the smaller one since we can always further threshold 
threshold_for_cut <- min(mean(threshold_collection),median(threshold_collection))

input_path <- paste0('/home/yiqun.chen/matchwhole_pc_remains_data/matchwhole_pc_remains_',parish,'/')

output_path <- paste0('/home/yiqun.chen/matchwhole_pc_remains_data/matchwhole_pc_remains_',parish,'/')

commandArgs <- function() c(threshold_for_cut,total_num_of_batches, chunk_len,type_of_match, parish, best_algo_param_path, input_path,output_path)

source('/home/yiqun.chen/matchwhole_pc_and_pc_remains_code/matchwhole_pc_remains_part2_automated.R')

#############################################################################
#############  Now we merge the pairs from pc and pc remains  #############
#############################################################################

pc_pairs_path = paste(matchwhole_output_path ,'myMatches_best_method_Matched_',parish,'_',type,'.RData',sep='')

output_path <- paste0('/home/yiqun.chen/matchwhole_pc_remains_data/matchwhole_pc_remains_',parish,'/')

pc_remains_pairs_path = paste(output_path,'pc_remains_myMatches_best_method_Matched_',parish,'_',type,'.RData',sep='')


commandArgs <- function() c(pc_pairs_path,pc_remains_pairs_path,type_of_match, parish,output_path)

source('/home/yiqun.chen/matchwhole_pc_and_pc_remains_code/combine_pc_pc_remains_pairs_automated.R')


#############################################################################


