################ locations and parameters to specify ##########################


parish = 'bugamba' # fill in parish accordingly

input_data_path = '/home/yiqun.chen/prematch_dataset/'

# (currently not in use since we have datasets on local drives)
output_path = '/home/yiqun.chen/trainset_data/'

# fill in output path accordingly


################ remember to change this from time to time if necessary ##############
commandArgs <- function() c(input_data_path,output_path,parish)

############# This part should generate a prematch trainset file #################

source('/home/yiqun.chen/trainset_code/trainset_yc_automation.R')

#############   This part should generate a prelim golden set  file   #################

source('/home/yiqun.chen/trainset_code/trainset_automation_2.R')

############# We correct the golden sets and this generates the corrected golden pair##
# source('~/Desktop/yc_june_new_code/automation/train_set_automation_part3.R')



#############  Now we are ready to do matchwhole   #################