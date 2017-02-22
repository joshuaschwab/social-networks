################ locations and parameters to specify ##########################

parish = 'sibuoche' # fill in parish accordingly 

train_set_data_path = '/home/yiqun.chen/trainset_data/'

input_data_path = '/home/yiqun.chen/goldenset_data/'

# (currently not in use since we have datasets on local drives)
output_path = '/home/yiqun.chen/goldenset_outputs/' 

# fill in output path accordingly 

################ remember to change this from time to time if necessary ##############
param.n = 1000
commandArgs <- function() c(input_data_path,output_path,parish, param.n)

############# This part should generate a parameter selection #################

source('/home/yiqun.chen/goldenset_code/train_set_goldenset_parameter.R')

############# This paer should produce match.l from train.link  #################

parameter_input_path = paste(output_path,parish,'_param_',param.n*7,'.RData',sep='')

commandArgs <- function() c(input_data_path,output_path,parish, parameter_input_path, train_set_data_path)

source('/home/yiqun.chen/goldenset_code/train_set_goldenset_match_l.R')

#############  Now we are ready to do match.l   #################


match_l_input_file ='/home/yiqun.chen/goldenset_outputs/'
match_l_output_file ='/home/yiqun.chen/goldenset_outputs/'

commandArgs <- function() c(match_l_input_file,match_l_output_file,parish)

source('/home/yiqun.chen/goldenset_code/train_set_goldenset_combine_match.R')


#############   Now we can assess the matchwhole results    #################

clean_golden_set_input_file ='/home/yiqun.chen/goldenset_data/'
match_l_input_file = '/home/yiqun.chen/goldenset_outputs/'
final_output_file ='/home/yiqun.chen/goldenset_outputs/'

commandArgs <- function() c(clean_golden_set_input_file,match_l_input_file,final_output_file,parish)

source('/home/yiqun.chen/goldenset_code/use_golden_set_automated.R')













