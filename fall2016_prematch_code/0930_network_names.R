
# we do not care about the cases where chc has no names
# library(stringdist)
source('~/Desktop/YC_code/CleanNames_methods.R')
load('~/Dropbox/Network/old_code/Given_names.RData')


SNA_new_dataset <- read.csv2('~/Dropbox/Network/BL SN DATA/Wen DataRequest NameMatching SNA Var 16Sep2016.csv',sep=",",header=T)
# 
SNA_new_dataset_bugamba <- SNA_new_dataset[SNA_new_dataset$community_name=='Bugamba',]

parish = 'bugamba'




SNA_new_dataset_bugamba$tracking_id = paste0(SNA_new_dataset_bugamba$hhid,'-',SNA_new_dataset_bugamba$linenum)



SNA_new_dataset_bugamba$othername_census[!is.na(as.numeric(as.character(SNA_new_dataset_bugamba$othername_census)))] = ""
SNA_new_dataset_bugamba$participantsname_chc[!is.na(as.numeric(as.character(SNA_new_dataset_bugamba$participantsname_chc)))] = ""
SNA_new_dataset_bugamba$participantsname_trk[!is.na(as.numeric(as.character(SNA_new_dataset_bugamba$participantsname_trk)))] = ""
SNA_new_dataset_bugamba$participantsname_census[!is.na(as.numeric(as.character(SNA_new_dataset_bugamba$participantsname_census)))] = ""


bugamba_name_census = as.character(SNA_new_dataset_bugamba$participantsname_census)
bugamba_name_chc = as.character(SNA_new_dataset_bugamba$participantsname_chc)
bugamba_name_track = as.character(SNA_new_dataset_bugamba$participantsname_trk)
othername_census = as.character(SNA_new_dataset_bugamba$othername_census)
  
# this function concatenates the MIN/NYAR nickname to prevent
# possible ambiguity when we combine different name fields together

combine_nickname <- function(name_list){
  
  name_list <- unname(sapply(name_list,function(x)gsub(" +",' ',x)))
  name_list <- unname(sapply(name_list,function(x)gsub("^MIN ",' MIN-',x)))
  name_list <- unname(sapply(name_list,function(x)gsub("^WUON ",' WUON-',x)))
  name_list <- unname(sapply(name_list,function(x)gsub("^NYAR ",' NYAR-',x)))
  name_list <- unname(sapply(name_list,function(x)gsub("^NYA ",' NYA-',x)))
  name_list <- unname(sapply(name_list,function(x)gsub(" MIN ",' MIN-',x)))
  name_list <- unname(sapply(name_list,function(x)gsub(" WUON ",' WUON-',x)))
  name_list <- unname(sapply(name_list,function(x)gsub(" NYAR ",' NYAR-',x)))
  name_list <- unname(sapply(name_list,function(x)gsub(" NYA ",' NYA-',x)))
  modified_name_list <- name_list
  return(modified_name_list)
}

bugamba_name_census <- combine_nickname(bugamba_name_census)
bugamba_name_chc <- combine_nickname(bugamba_name_chc)
bugamba_name_track <- combine_nickname(bugamba_name_track)
othername_census <- combine_nickname(othername_census)

bugamba_merged_pariticpant_name = paste(bugamba_name_census,bugamba_name_chc,bugamba_name_track,othername_census)
bugamba_merged_pariticpant_name = lapply(bugamba_merged_pariticpant_name,function(x)(strsplit(x, split = " ")))
bugamba_merged_pariticpant_name = sapply(bugamba_merged_pariticpant_name,function(x)unique(unlist(x)))
bugamba_merged_pariticpant_name = sapply(bugamba_merged_pariticpant_name, function(x) paste(x,collapse = " "))
bugamba_merged_pariticpant_name = unlist(bugamba_merged_pariticpant_name)


# sanity check on the max length names
max(sapply(bugamba_merged_pariticpant_name,function(x)length(unlist(strsplit(x,split = " ")))))


SNA_new_dataset_bugamba$name_merged <- bugamba_merged_pariticpant_name



#save(SNA_new_dataset_bugamba,file='~/Dropbox/Network/10_10_final_pairs/09_28_new_bugamba_name_fixation.RData')





nContacts <- 6
snType <- c('free','emot','food','money','health')
snInfo <- c('name','age','village')
snCols <- do.call('paste',c(expand.grid(apply(expand.grid(snType,1:nContacts),1,paste,collapse=''),snInfo,stringsAsFactors = F),sep='_'))


# customized function for this cleaned ver
ToLongContactForm <- function(wide.df.list, snType, nContacts, snInfo=c('name','age','village'),emptyRef='name'){
  
  long.df <- do.call('rbind',lapply(snType,function(sn){
    sn.df <- do.call('rbind',lapply(1:nContacts,function(k){
      snk.df <- do.call('rbind',lapply(names(wide.df.list),function(src){
        source.df <- wide.df.list[[src]]
        ret.df <- source.df[c('SEARCHID',paste(paste(paste('sna',sn,sep=''),k,sep=''),snInfo,sep='_'))]
        names(ret.df)[-1] <- snInfo
        # non.empty <- is.na(as.numeric(ret.df[[emptyRef]]))  ## takeout empty names
        ret.df$sn <- sn
        ret.df$rank <- k
        ret.df$contactTypeID <- paste(ret.df$SEARCHID,sn,k,sep='_')
        ret.df$src <- src
        ret.df$contactTypeIDwsrc <- paste(ret.df$contactID,src,sep='_')
        return(ret.df)#ret.df[non.empty,]
      }))
      return(snk.df)
    }))
    dup <- duplicated(do.call('paste',sn.df[c('SEARCHID',snInfo,'sn')]))  ## same sn, but duplicated contact for that person
    print(table(dup))
    return(sn.df[!dup,])}))
  return(long.df)
}

GetUniqueParticipants <- function(wide.df.list){
  ret.df <- wide.df.list$census[c('name_merged','age','sex','village_name','tracking_id')]
  names(ret.df)[1:5] <- c('name','age','sex','village','tracking_id')
  ret.df$type <- 'p'
  
  ret.df <- ret.df[is.na(as.numeric(as.character(ret.df$name))),] ## return those with non-numeric name
  ret.df[c('name','village')] <- ToUpper(ret.df[c('name','village')])
  return(ret.df)
}



all.id <- data.frame(SEARCHID=unique(c(SNA_new_dataset_bugamba$tracking_id)))
SNA_new_dataset_bugamba$SEARCHID <- SNA_new_dataset_bugamba$tracking_id
sn.c.long <- ToLongContactForm(list(census=SNA_new_dataset_bugamba),snType=snType,nContacts,snInfo)
sn.c.long$name <- as.character(sn.c.long$name)
sn.c.long$village <- as.character(sn.c.long$village)

sn.c.long = sn.c.long[is.na(as.numeric(as.character(sn.c.long$name))),]
sn.c.long$age[sn.c.long$age<0] <- NA

# fix the contact name in the same way

sn.c.long$name <- combine_nickname(sn.c.long$name)



sn.c.uniq <- GetUniqueContacts(sn.c.long)

sn.c.uniq$contactID <- GetContactUniqID(sn.c.uniq)


sn.p.uniq<- GetUniqueParticipants(list(census=SNA_new_dataset_bugamba))



clean.p.uniq <- CleanFields(sn.p.uniq,str.fields = c('name','village'),int.fields=c('sex','age'))

clean.c.uniq <- CleanFields(sn.c.uniq, str.fields = c('name','village'),int.fields=c('age'))


MakeHonorSuffixTables.bugamba <- function(){
  
  honorific.df <- data.frame(str=unlist(list(REVEREND=c('REVEREND','REVERAND','RV','REV','RE','FATHER','REVERANDFATHER','REVERAND FATHER'),PASTOR=c('PASTOR','PASTER','PR'),DOCTOR=c('DOCTOR','DR'),
                                             MAMA='MAMA',MAMAJUNIOR=c('MAMAJUNIOR','MAMA JUNIOR'), BABA='BABA',MRS=c('MRS','MRS RE','MRS REV'),MADAM='MADAM',RS='RS',CAPTAIN=c('CAPTAIN','CAPTIAN'),
                                             GENERAL=c('GENERAL','GRAL'),GENERALSECRETARY=c('GENERALSECRETARY','GENERAL SERETARY'),DIRECTOR=c('THE D E O OF KIBU','DIR'),
                                             CHAIRMAN=c('CHAIRMAN','CHAIRMAN LC1','CHAIRMAN LC 1','CHAIR PE','LC1 CMAN','LC1 CHAIRMAN','CHAIR MAN','CHAIMAN'),
                                             PROFESSOR=c('PROFESSOR','PROF','PROFESOR'),                                          
                                             TEACHER=c('TEACHER'),COUNCILLOR=c('COUNCILLOR','COUNSILLOR'))),stringsAsFactors=F)
  
  
  honorific.df$categ <- gsub("\\d", "", row.names(honorific.df)) 
  
  suffix.df <- data.frame(str=unlist(list(JUNIOR=c('JUNIOR','JR'),SENIOR=c('SENIOR','SR'),
                                          I=c('I','1','ONE', 'FIRST','DA FIRST','THE FIRST'),II=c('II','2','SECOND','TWO','DA SECOND','THE SECOND'),
                                          III=c('III','3','THREE','THIRD','DA THIRD','THE THIRD'),IV=c('IV','4','FOUR','FOURTH','DA FOURTH','THE FOURTH'),
                                          SON=c('SON'),DAUGHTER=c('DAUGHTER','DUAGHTER'))),stringsAsFactors=F)
  
  suffix.df$categ <- gsub("\\d", "", row.names(suffix.df)) 
  
  nicknames <- c('MIN','NYA','NYAR','WUON')
  return(list(honor=honorific.df, suffix=suffix.df,nicknames=nicknames))
}

temp <- MakeHonorSuffixTables.bugamba()
honorific.df <- temp$honor
suffix.df <- temp$suffix

clean.c.uniq[c('name.clean','name.honor','name.suffix')] <- GetHonorificAndSuffix(clean.c.uniq$name,honorific.df,suffix.df)
clean.c.uniq[c('parenthe.clean','parenthe.honor','parenthe.suffix')] <- GetHonorificAndSuffix(clean.c.uniq$parenthe,honorific.df,suffix.df)
clean.p.uniq[c('name.clean','name.honor','name.suffix')] <- GetHonorificAndSuffix(clean.p.uniq$name,honorific.df,suffix.df)

clean.p.uniq[c('parenthe.clean','parenthe.honor','parenthe.suffix')] <- GetHonorificAndSuffix(clean.p.uniq$parenthe,honorific.df,suffix.df)

clean.p.uniq[c('othername.clean','othername.honor','othername.suffix')] <- GetHonorificAndSuffix(clean.p.uniq$othername,honorific.df,suffix.df)

clean.c.uniq[c("honorific",'suffix')] <- GroupHonorificAndSuffix(clean.c.uniq,cols.list = list(honorific=c('name.honor','parenthe.honor'),suffix=c('name.suffix','parenthe.suffix')), ref.list=list(honorific=honorific.df,suffix=suffix.df))

clean.p.uniq[c("honorific",'suffix')] <- GroupHonorificAndSuffix(clean.p.uniq,cols.list = list(honorific=c('name.honor','parenthe.honor'),suffix=c('name.suffix','parenthe.suffix')), ref.list=list(honorific=honorific.df,suffix=suffix.df))

clean.c.uniq$name.len <- GetWordLength(clean.c.uniq$name.clean)
clean.p.uniq$name.len <- GetWordLength(clean.p.uniq$name.clean)
clean.p.uniq2 <- NULL
n.has.nick <- NULL
clean.p.uniq1 <- NULL

n.has.nick <- grep("^MIN |^NYA |^NYAR |^WUON ",clean.c.uniq$name)
clean.c.uniq$name.clean[n.has.nick] <- clean.c.uniq$name[n.has.nick]

p.has.2nick <- grep(' OR MIN | OR NYA | OR WUON | OR NYAR ',clean.p.uniq$othername)
clean.p.uniq2 <- clean.p.uniq[p.has.2nick,]
clean.p.uniq2$name.clean <- gsub("^.*? OR ","",clean.p.uniq2$othername)
#clean.p.uniq2$othername.clean  <- ''


# try NAME
p.has.nick <- grep("MIN |NYA |NYAR |WUON ",clean.p.uniq$name)
clean.p.uniq1 <- clean.p.uniq[p.has.nick,]
clean.p.uniq1$name.clean <- gsub( "OR.*$", "", clean.p.uniq$othername[p.has.nick]) 


# we need to break the function somehow! :0 
# now we take 4 components and if there atr things beyond four we dump them into othername.clean


max(clean.p.uniq$name.len)
# we can break up the names 

subset_clean_p_uniq <- clean.p.uniq[clean.p.uniq$name.len>4,]
clean.p.uniq.break <- BreakName(subset_clean_p_uniq,'name.clean',4)


clean.p.uniq <- BreakName(clean.p.uniq,'name.clean',4)
clean.c.uniq$othername.clean <- clean.c.uniq$parenthe.clean
clean.c.uniq$parenthe.clean <- ''
clean.c.uniq <- BreakName(clean.c.uniq,'name.clean',4)

#clean.p.uniq$name.len>4


clean.c.uniq$sex <- NA
clean.c.uniq$sex[clean.c.uniq$honorific %in% c('MAMA','MRS','MADAM')] <- 2

# fixed !is.na
AssignSexByName <- function(c.df, f.names, m.names){
  no.sex <- is.na(c.df$sex)
  ret.df <- data.frame(lapply(c.df[c('name1','name2','name3','name4')],function(name){
    in.m=in.f <- rep(FALSE,length(name))
    nonempty <- name!='' & !is.na(name)
    in.m[nonempty] <- name[nonempty] %in% m.names #has.sim(name[nonempty], m.names, str.bound) #
    in.f[nonempty] <- name[nonempty] %in% f.names #has.sim(name[nonempty], f.names, str.bound) #
    this.sex <- (1*(in.m & !in.f) + -1*(!in.m & in.f))  ## if not in either, or in both, then inconclusive, then contribute 0
  }))
  female <- apply(ret.df,1,min)==-1 & apply(ret.df,1,max)==0 ## female=all -1, or 0
  male <- apply(ret.df,1,min)==0 & apply(ret.df,1,max)==1 ## male= all 1 or 0
  c.df$sex[no.sex & female] <- 2
  c.df$sex[no.sex & male] <- 1
  return(c.df$sex)
}


if('n.has.nick' %in% ls()){
  clean.c.uniq$sex[-n.has.nick] <- AssignSexByName(clean.c.uniq[-n.has.nick,],f.names,m.names)} else{
    clean.c.uniq$sex <- AssignSexByName(clean.c.uniq,f.names,m.names)
  }


prematch.p <- IncorporateOthername(clean.p.uniq,'parenthe.clean',remnants.var = 'othername.prematch')

prematch.p <- IncorporateOthername(clean.p.uniq,'othername.clean',remnants.var = 'othername.prematch')

prematch.c <- IncorporateOthername(clean.c.uniq,'othername.clean',remnants.var = 'othername.prematch')

prematch.p$SEARCHID <- prematch.p$tracking_id

prematch.p.before.long.time.permute <- prematch.p

generate_permutated_rows <- function(input_row){
  
  
  rep.row<-function(x,n){
    matrix(rep(x,each=n),nrow=n)
  }
  
  
  corrected_row = unlist(strsplit(input_row$name,split=" "))
  corrected_row = unique(corrected_row)
  if (length(corrected_row)>4){
    corrected_names = combn(corrected_row,4,FUN=function(x)paste(x,collapse = " "))
    corrected_othernames = unname(unlist(sapply(corrected_names,function(x)paste(corrected_row[!corrected_row%in%unlist(strsplit(x,split = " "))],collapse = " "))))    #combn(corrected_row,4,FUN=function(x)corrected_row[!corrected_row%in%unlist(strsplit( paste(x,collapse = " "),sep = " "))])
    return_val = list(corrected_names, corrected_othernames)
    ouput_data = rep.row(input_row,n = choose(length(corrected_row),4))
    ouput_data = as.data.frame(ouput_data)
    names(ouput_data) = names(input_row)
    break_up_clean_name = combn(corrected_row,4)
    
    for (i in c(1: choose(length(corrected_row),4) ) ){
      # we will keep the original name
      #ouput_data[i,]$name = corrected_names[i]
      #ouput_data[i,]$name.clean = corrected_names[i]
      ouput_data[i,]$othername.clean = corrected_othernames[i]
      ouput_data[i,]$othername.prematch = corrected_othernames[i]
      ouput_data[i,]$name1 = break_up_clean_name[1,i]
      ouput_data[i,]$name2= break_up_clean_name[2,i]
      ouput_data[i,]$name3= break_up_clean_name[3,i]
      ouput_data[i,]$name4= break_up_clean_name[4,i]
      
    }
    return(ouput_data)
  }
  else{
    return(input_row)
  }
  
}


subset_prematch_p = prematch.p[prematch.p$name.len>4,]
new_split_name_prematch_p = subset_prematch_p[1,]

for (i in c(1:nrow(subset_prematch_p))){
  current_split_name = generate_permutated_rows(subset_prematch_p[i,])
  #cat(dim(current_split_name),'\n')
  new_split_name_prematch_p = rbind(new_split_name_prematch_p,current_split_name)
}
new_split_name_prematch_p = new_split_name_prematch_p[-1,]

prematch.p <- prematch.p[prematch.p$name.len<=4,]
prematch.p <- rbind(prematch.p,new_split_name_prematch_p)
prematch.p2 <- GetNamePermutations(prematch.p,ref.id='SEARCHID') ## permute all




# now our prematch is good to go but we need to find out new pairs (prematch.c that are not in the old ones)

#missing_contact_ID <- prematch.c$contactID[!(prematch.c$contactID %in% sn.c.uniq.old$contactID)]

#missing_id_position <- which(!(prematch.c$contactID %in% sn.c.uniq.old$contactID)==TRUE)#

#intersecting_intersect(missing_id_position,n.has.nick)



if(is.null(n.has.nick)|(is.integer(n.has.nick)&length(n.has.nick)==0)){
  prematch.c2 <- GetNamePermutations(prematch.c,ref.id='contactID') ## 131420 unique contacts, 64239 contacts
}

if(!is.null(n.has.nick)&(is.integer(n.has.nick)&length(n.has.nick)>0)){
  prematch.c2 <- GetNamePermutations(prematch.c[-n.has.nick,],ref.id='contactID') ## 131420 unique contacts, 64239 contacts
  clean.c.uniq$othername.prematch <- ''
  clean.c.uniq$origName <- clean.c.uniq$name.clean
  clean.c.uniq$refID <- clean.c.uniq$contactID
  clean.c.uniq$uniqueID <- paste(clean.c.uniq$contactID,'n',sep='_')
  prematch.c2 <- rbind(prematch.c2,clean.c.uniq[n.has.nick, names(prematch.c2)])
}


parish = 'bugamba'
for (i in 1:length(names(prematch.p))){
  prematch.p[,i] <- unlist(prematch.p[,i])
}
for (i in 1:length(names(prematch.p2))){
  prematch.p2[,i] <- unlist(prematch.p2[,i])
}



prematch.c$village <- unlist(prematch.c$village)
prematch.c2$village <- unlist(prematch.c2$village)
prematch.p.before.long.time.permute$village <-unlist(prematch.p.before.long.time.permute$village)

save(prematch.p.before.long.time.permute,prematch.p,prematch.c,prematch.p2,prematch.c2,file=paste('~/Dropbox/network_yiqun/fall2016_prematch/permutation_fix/myPrematch_',parish,'.RData',sep=''))
save(clean.p.uniq,clean.c.uniq,honorific.df, suffix.df,f.names, m.names, file=paste('~/Dropbox/network_yiqun/fall2016_prematch/Cleandata/myCleanData_',parish,'.RData',sep=''))
villages.study <- sort(unlist(unique(prematch.p$village)))

save(SNA_new_dataset_bugamba,parish,villages.study, file=paste('~/Dropbox/network_yiqun/fall2016_prematch/Cleandata/myRawData_',parish,'.RData',sep=''))











