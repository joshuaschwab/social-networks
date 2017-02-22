LoadSASRawData <- function(filename){
  df <- read.sas7bdat(filename)
  df <- TurnFactorsToChar(df)
  df$SEARCHID <- paste(df$hhid,df$linenum,sep='-')
  if(!any(duplicated(df$SEARCHID))) rownames(df) <- df$SEARCHID
  return(df)
}
checkCols <- function(df1,df2,compare.vars,id.var){
  common.id <- intersect(df1[[id.var]],df2[[id.var]])
  sapply(compare.vars,function(ss)sum(df1[common.id,ss]!=df2[common.id,ss] & df1[common.id,ss]!= -9 & df2[common.id,ss]!= -9))
}


### turn all contacts into one long big data.frame
# @wide.df raw data with contact info in wide format: each row per participant, has nContacts contacts and their info.
# contact infom in wide.df are networTypeInfoContactn, e.g. snafreename1, snafree=networkType,name=contactInfo,1=contactn
ToLongContactForm <- function(wide.df.list, snType, nContacts, snInfo=c('name','age','village'),emptyRef='name'){
  
  long.df <- do.call('rbind',lapply(snType,function(sn){
    sn.df <- do.call('rbind',lapply(1:nContacts,function(k){
      snk.df <- do.call('rbind',lapply(names(wide.df.list),function(src){
        source.df <- wide.df.list[[src]]
        ret.df <- source.df[c('SEARCHID',paste(paste(paste('sna',sn,sep=''),k,sep=''),snInfo,sep='_'))]
        names(ret.df)[-1] <- snInfo
        non.empty <- is.na(as.numeric(ret.df[[emptyRef]]))  ## takeout empty names
        ret.df$sn <- sn
        ret.df$rank <- k
        ret.df$contactTypeID <- paste(ret.df$SEARCHID,sn,k,sep='_')
        ret.df$src <- src
        ret.df$contactTypeIDwsrc <- paste(ret.df$contactID,src,sep='_')
        return(ret.df[non.empty,])
      }))
      return(snk.df)
    }))
    dup <- duplicated(do.call('paste',sn.df[c('SEARCHID',snInfo,'sn')]))  ## same sn, but duplicated contact for that person
    print(table(dup))
    return(sn.df[!dup,])}))
  return(long.df)
}
### assign missing values to NA
### take out non-alphanumeric chars and trim leading and trailing spaces form strs
CleanFields <- function(df, str.fields, int.fields){
  df[int.fields] <- data.frame(lapply(int.fields,function(ii){
    df[[ii]][df[[ii]]<0] <- NA
    return(df[[ii]])
  }))
  df[str.fields] <- data.frame(lapply(str.fields,function(ss){
    df[[ss]][!is.na(as.numeric(df[[ss]]))] <- ''
    
    ## 0. add a space after dot
    df[[ss]] <- gsub("\\.+", ". ", df[[ss]])
    ## only take out puctuations if not name, and trim trailing and leading spaces
    if(ss=='village' | ss=='othername'){   
      df[[ss]] <- gsub("^ *|(?<= ) | *$", "", gsub("[^a-zA-Z0-9 ]","",df[[ss]]),perl=T)
    }
    return(df[[ss]])}),stringsAsFactors=F)
  
  ### clean name ####
  ## 1. extract parenthesis
  parenthe.cmp <- gsub("\\[|\\]|\\(|\\)", "", regmatches(df$name, gregexpr("\\(.*?\\)|\\[.*?\\]",df$name)))
  parenthe.cmp[parenthe.cmp=="character0"] <- ''
  has.parenth <- parenthe.cmp != ''
  df$name[has.parenth] <- gsub( " *\\(.*?\\) *| *\\[.*?\\] *", " ", df$name[has.parenth])
  ## 2. delete non-alphanum chars and leading or traling spaces
  df$name <- gsub("^ *|(?<= ) | *$", "", gsub("[^a-zA-Z0-9 ]","",df$name),perl=T)
  df$parenthe <- gsub("^ *|(?<= ) | *$", "", parenthe.cmp,perl=T)
  return(df)
}

AssignSexByName <- function(c.df, f.names, m.names){
  no.sex <- is.na(c.df$sex)
  ret.df <- data.frame(lapply(c.df[c('name1','name2','name3','name4')],function(name){
    in.m=in.f <- rep(FALSE,length(name))
    nonempty <- name!=''
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

GetUniqueParticipants <- function(wide.df.list){
  ret.df <- wide.df.list$census[c('participantsname_census','othername_census','age_census','sex_census','village_name','SEARCHID')]
  names(ret.df)[1:5] <- c('name','othername','age','sex','village')
  ret.df$src <- 'census'
  srcs <- setdiff(names(wide.df.list),'census')
  for(src in srcs){
    temp.df <- wide.df.list[[src]]
    if(!('village_name' %in% names(temp.df))) temp.df$village_name <- NA
    if(!('othername' %in% names(temp.df))) temp.df$othername <- NA
   # if(!('sex' %in% names(temp.df))) temp.df$sex <- NA
    temp.df <- temp.df[!(temp.df$SEARCHID %in% ret.df$SEARCHID),c(paste(c('participantsname','age','sex'),src,sep='_'),'village_name','othername','SEARCHID')]
   ## if chc, when no villagE_name, may have village
    if(src=='chc'){
      wide.village <- wide.df.list[[src]]$village[!(wide.df.list[[src]]$SEARCHID %in% ret.df$SEARCHID)]
      has.village <- (temp.df$village_name=='' | !is.na(as.numeric(temp.df$village_name))) & !(wide.village=='' | !is.na(as.numeric(wide.village)))
      temp.df$village_name[has.village] <- wide.village[has.village]
    }
    names(temp.df)[1:4] <- c('name','age','sex','village')
    if(nrow(temp.df)>0) {
      temp.df$src <- src
    ret.df <- rbind(ret.df,temp.df[names(ret.df)])}
  }
  ret.df$type <- 'p'
  ret.df <- ret.df[is.na(as.numeric(ret.df$name)),] ## return those with non-numeric name
  ret.df[c('name','othername','village')] <- ToUpper(ret.df[c('name','othername','village')])
  return(ret.df)
}
## @uniq.c.df: one row per contact person
GetContactUniqID <- function(uniq.c.df){
  namer.id <- as.numeric(table(uniq.c.df$SEARCHID))
  names(namer.id) <- names(table(uniq.c.df$SEARCHID))
  id <- unlist(sapply(names(namer.id),function(xx){paste(xx,1:(namer.id[[xx]]),sep='-')}))
  c.rows <- unlist(sapply(names(namer.id),function(xx)which(uniq.c.df$SEARCHID==xx)))
  uniq.c.df[c.rows,'id'] <- id
  return(uniq.c.df$id)
}
### @long.c.df: SEARCHID, name, age, village, sn, src
GetUniqueContacts <- function(long.c.df){
  long.c.df$persons <- (do.call('paste',c(long.c.df[c('SEARCHID','name','age','village')],sep='.')))
  uniq.df <- long.c.df[!duplicated(long.c.df$persons),]
  dup.persons <- long.c.df$persons[duplicated(long.c.df$persons)]
  dup.df <- sapply(unique(dup.persons),function(pp){
    paste(do.call('paste',c(long.c.df[long.c.df$persons==pp,c('sn','rank','src')],sep='.')),collapse='_')
  })
  names(dup.df) <- unique(dup.persons)
  nondup.persons <- unique(setdiff(long.c.df$persons,dup.persons))
  uniq.df$snTypes <- NA
  uniq.df$snTypes[match(nondup.persons,uniq.df$persons)] <- do.call('paste',c(uniq.df[match(nondup.persons,uniq.df$persons),c('sn','rank','src')],sep='.'))
  uniq.df$snTypes[match(unique(dup.persons),uniq.df$persons)] <- dup.df
  uniq.df$type <- 'c'
  uniq.df[c('name','village')] <- ToUpper(uniq.df[c('name','village')])
  uniq.df$village[!is.na(as.numeric(uniq.df$village))] <- ''
  return(uniq.df)
}

CorrectVar <- function(var,wrong,correct){
  var[var==wrong] <- correct
  return(var)
}

StandardizeVillage <- function(data, standard.df){
  for(ind in 1:nrow(standard.df)){
    data$village <- CorrectVar(data$village,standard.df$old[ind],standard.df$standard[ind])
  }
  return(data)
}



### Get a distance matrix between two vector of strings
### will convert both to upper case
GetStringDistMat <- function(row.str, col.str=NA, method='lv', p=NULL, row.names=NA, col.names=NA,toUpper=TRUE){
  if(is.na(col.str)) col.str <- row.str
  if(toUpper){
    row.str <- toupper(row.str)
    col.str <- toupper(col.str)
  }
  if(is.null(p))mat <- stringdistmatrix(row.str,col.str,method=method)
  else mat <- stringdistmatrix(row.str,col.str,method=method,p=p) 
  mat <- as.data.frame(mat)
  if(is.na(row.names)) rownames(mat) <- row.str
  if(is.na(col.names)) colnames(mat) <- col.str
  mat
}

### for each column of df, return either rows that satisfy the cutoff, or empty
### value: data frame: rows = cols of df, cols = cut off result for each col 
GetDFCutOffMin <- function(dist.df,cutoff, row.str=NA, method, upperOnly=FALSE){
    mins <- apply(dist.df,2,min)
    if(is.na(row.str)) row.str <- row.names(dist.df)
    ret <- names(dist.df)  ## if =0, or no similar, then no correct
    ret[mins<=cutoff] <- sapply(which(mins<=cutoff),function(ii){
      cc <- dist.df[[ii]]
      row.str <- row.str[which(cc==min(cc))]
      if(length(row.str)>1){
        if(!is.na(method)) {
          cc <- stringdist(ret[ii],row.str,method=method)
          if(sum(cc==min(cc))>1){return(ret[ii])}
          else row.str[cc==min(cc)]
        }
        return(ret[ii])
      }
      return(row.str)
       })
    return(data.frame(cbind(orig=names(dist.df),correct=ret),stringsAsFactors=F))
    
#   if(is.na(row.str)) row.str <- row.names(df)
#   
#   col.vect <- 1:ncol(df)
#   if(upperOnly) col.vect <- 2:ncol(df)
#   
#   ret.df <- data.frame(t(data.frame(lapply(col.vect,function(col.ind){
#     
#     if(upperOnly){ 
#       
#       rows.ind  <- 1:(col.ind-1)
#     }
#     else rows.ind <- 1:nrow(df)
#       
#     vals <- df[rows.ind,col.ind]
#       
#     if(min(vals)>cutoff){return(c('-1',NA))}
#     
#     min.set <- which(vals==min(vals))
#     min.val <- paste(vals[min.set],collapse='/')
#     min.row <- paste(row.str[rows.ind][min.set],collapse='/')
#     
#     return(c(min.val,min.row))}))))
#   
#   names(ret.df) <- c('dist','matches')
#   ret.df$orig <- names(df)[col.vect]
#   row.names(ret.df) <- NULL
#   ret.df
}

GetVillagesCorrect <- function(study.v,contacts.v,method,cutoff,p){
  villageDist <- GetStringDistMat(row.str=study.v,col.str=contacts.v,method=method[1],p)
  print(setequal(names(villageDist)[apply(villageDist,2,min)==0],study.v))
  villages.correct <- GetDFCutOffMin(villageDist,cutoff =cutoff,method=method[2])
}
CorrectVillage <- function(df.v, villages.correct){
  for(ii in 1:nrow(villages.correct)){
    orig <- villages.correct$orig[ii]
    correct <- villages.correct$correct[ii]
    df.v[df.v==orig] <- correct
  }
  return(df.v)
}

## remove last n characters from each string in x
removeLastnChar <- function(x, n){
  sapply(x, function(xx)
    substr(xx, 1, (nchar(xx)-n))
  )
}

## get last n chars of a string
substrLastn <- function(x,n=3){
  sub('.*(?=.{3}$)', '', test, perl=T)
}
##Get non-word ends
GetEnds <- function(name.v){
  bad.starts.ind <- grep('^\\W',name.v)
  bad.starts <- unique(substring(name.v[bad.starts.ind],1,1))
  bad.starts.l <- lapply(bad.starts,function(s){which(substring(name.v,1,1)==s)})
  names(bad.starts.l) <- bad.starts
  
  bad.ends.ind <- grep('*\\W$',name.v)
  bad.ends <- unique(sub('.*(?=.{1}$)', '', name.v[bad.ends.ind], perl=T))
  bad.ends.l <- lapply(bad.ends, function(s){which(sub('.*(?=.{1}$)', '', name.v, perl=T)==s)})
  names(bad.ends.l) <- bad.ends
  return(list(start=bad.starts.l,ends=bad.ends.l))
}

GetWordLength <- function(x){
  sapply(strsplit(x,' '), length)
}

TrimSpace <- function(df){
  data.frame(lapply(df,function(x)gsub(' {2,}',' ',x)),stringsAsFactors=F)
}
ToUpper <- function(df){
  data.frame(lapply(df,toupper),stringsAsFactors=F)
}


TrimEnds <- function(name.v, nChar, start=TRUE){
  
  if(start)
    new <- substring(name.v, nChar+1)
  else
    new <- removeLastnChar(name.v,nChar)
  
  write(date(),file=paste('~/SNA/code/outputlogs/TrimEndsLog.txt'),append=TRUE)
  write.table(data.frame(orig=name.v,new=new ),file=paste('~/SNA/code/outputlogs/TrimEndsLog.txt'),append=TRUE,sep='\t',row.names=FALSE)
  
  return(new)
}

GetMultiBlockLinkage <- function(data1,data2, blockflds, strcmp, intcmp, exclude){
  
  link <- vector('list',1)
  for(ind in 1:length(blockflds)){
    cat(date(),blockflds[ind],'blocking\n')
    if(ind==1){
        link <- my.compare.linkage(data1,data2,blockfld=blockflds[ind],strcmp=strcmp,intcmp=intcmp,exclude=exclude)
          }else{
     new.pairs <- try({
       my.compare.linkage(data1,data2,blockfld=blockflds[ind],strcmp=strcmp,intcmp=intcmp,exclude=exclude)$pairs
     })
      if(is.data.frame(new.pairs))
      link$pairs <- rbind(link$pairs,new.pairs)
    }
  }
  dup.pairs <- duplicated(do.call('paste',c(link$pairs[c('id1','id2')],sep='.')))  ## pairs that are matched on more than one field, only need to keep one
  refID1 <- link$data1$refID[link$pairs$id1]
  refID2 <- link$data2$refID[link$pairs$id2]
  selfies <- refID1==refID2  ##pairs that are same record (only happens on CC matchess)
  cat(sum(selfies),'selfies\n')
  link$pairs <- link$pairs[!dup.pairs & !selfies,]
  return(link)
}

### extract honorific and suffix from given name.vect
GetHonorificAndSuffix <- function(name.vect,honor.df, suf.df){
  honor.df$len <- GetWordLength(honor.df$str)
  suf.df$len <- GetWordLength(suf.df$str)
  honor.df <- honor.df[order(honor.df$len,decreasing=T),]
  suf.df <- suf.df[order(suf.df$len,decreasing=T),]
  
  suf.vect <- rep('',length(name.vect))
  honor.vect <- rep('',length(name.vect))
  
  for(s.ind in 1:nrow(suf.df)){
    s <- suf.df$str[s.ind]
    for(pos in c('tail','exact')){
      pat <- switch(pos,tail=paste(' ',s,'$',sep=''), exact=paste('^',s,'$',sep=''))
    has.ind <- grep(pat,name.vect)
    if( (pos=='middle' | pos=='exact') & length(has.ind)>0){cat(pat,name.vect[has.ind],'\n')}
    suf.vect[has.ind] <- paste(suf.vect[has.ind],suf.df$categ[s.ind],sep='_')
    name.vect[has.ind] <- gsub(pat,'',name.vect[has.ind])
    }
#     has.exact <- which(name.vect==s)
#     if(length(has.exact)>0){
#       has.len1 <- which(sapply(name.vect,GetWordLength)==1)
#       if(length(has.len1)>0){
#         one.ind <- intersect(has.len1, has.exact)
#         suf.vect[one.ind] <- paste(suf.vect[one.ind],suf.df$categ[s.ind],sep='_')
#         name.vect[one.ind] <- gsub(s,'',name.vect[one.ind])
#       }}
  }
  
  for(s.ind in 1:nrow(honor.df)){
    s <- honor.df$str[s.ind]
    for(pos in c('tail','head','middle','exact')){
      pat <- switch(pos,tail=paste(' ',s,'$',sep=''), head=paste('^',s,' ',sep=''),
                    middle=paste(' ',s,' ',sep=''),exact=paste('^',s,'$',sep=''))
      has.ind <- grep(pat,name.vect)
      if( (pos=='middle' | pos=='exact') & length(has.ind)>0){cat(pat,name.vect[has.ind],'\n')}
      honor.vect[has.ind] <- paste(honor.vect[has.ind],honor.df$categ[s.ind],sep='_')
      if(pos!='middle') name.vect[has.ind] <- gsub(pat,'',name.vect[has.ind])
      else name.vect[has.ind] <- gsub(pat,' ',name.vect[has.ind])
    }
#     has.exact <-  which(name.vect==s)
#     if(length(has.exact)>0){
#       has.len1 <- which(GetWordLength(name.vect)==1)
#       if(length(has.len1)>0){
#     one.ind <- intersect(which(sapply(name.vect,GetWordLength)==1), grep(s,name.vect))
#     cat(name.vect[one.ind],'\n')
#     honor.vect[one.ind] <- paste(honor.vect[one.ind],honor.df$categ[s.ind],sep='_')
#     name.vect[one.ind] <- gsub(s,'',name.vect[one.ind])
#     }}
  }
  
  ### clean out trailing or leading '_'
  suf.vect <- gsub('^_|_$','',suf.vect)
  honor.vect <- gsub('^_|_$','',honor.vect)
  suf.vect <- unlist(lapply(strsplit(suf.vect,split='_',fixed=T),function(ss){
    if(length(ss)>0){return(paste(unique(ss),collapse='_'))}
    return('')
  }))
  honor.vect <- unlist(lapply(strsplit(honor.vect,split='_',fixed=T),function(ss){
    if(length(ss)>0){return(paste(unique(ss),collapse='_'))}
    return('')
  }))
  return(data.frame(clean.name=name.vect,honorific=honor.vect,suffix=suf.vect,stringsAsFactors = F))
  
}

### decide all honorifics and suffixes from name, other naem, parenthesis
### cols.list=list(honor=columsn that potential contain honor., suffix=columns that potentially contain suffix) 
####  
#### ref.list=list(honor=honor.df referece, suffix=suffix.df refernce)
GroupHonorificAndSuffix <- function(df,cols.list,ref.list){
  ret.df <- data.frame(lapply(names(cols.list),function(ss){
    cols <- cols.list[[ss]]
    ref.df <- ref.list[[ss]]
    ret <- do.call('paste',c(df[cols],sep=' '))
    ret <- gsub("^ *|(?<= ) | *$", "", ret,perl=T) ## trim spaces
    return(ret)}),stringsAsFactors=F)
  names(ret.df) <- names(cols.list)
  return(ret.df)
}

### break clean name into given n comps
BreakName <- function(df, name.var, cmps.n){
  cmps <- lapply(strsplit(df[[name.var]],split=' ',fixed=T),function(ss){
    if(length(ss)>cmps.n){
      ss[cmps.n+1] <- paste(ss[(cmps.n+1):length(ss)],collapse=' ')
      cat(ss,'\n')
    }
    else{
      ss <- c(ss, rep('',(cmps.n+1)-length(ss)))
    }
    return(ss)
  })
  for(ii in 1:cmps.n){
    cat(date(),ii,'\n')
    df[[paste('name',ii,sep='')]] <- sapply(cmps,function(ss)ss[ii])
  }
  if(!('othername.clean' %in% names(df))) df$othername.clean <- ''
  df$othername.clean[is.na(df$othername.clean)] <- ''
  df$othername.clean <- paste(df$othername.clean,sapply(cmps,function(ss)ss[cmps.n+1]),sep=' ')
  df$othername.clean <- gsub("^ *|(?<= ) | *$", "", df$othername.clean,perl=T) 
  return(df)
}



### returns vector of possible prefix and suffix, and number of name components without pref or suff, and substract them from anem
### @name.v name vector to be separated, no numerical strings, no non-word end characters
### 
SeparatePrefixSuffix <- function(name.v, prefix.df,suffix.df){
  
  fix.df <- rbind(data.frame(prefix.df,type='pre'),data.frame(suffix.df,type='suf'))
  
  names.l <- strsplit(name.v,split=' ',fixed=T)
  names.len <- sapply(names.l,length)
  start <- sapply(names.l,function(s)s[1])  ## first word of name
  end <- sapply(names.l,function(s){
    if(length(s)==0) return(NA)
    s[length(s)]}) ## last word of name
  
  start.df <- GetPSfix(start,fix.df)
  end.df <- GetPSfix(end,fix.df)
  
  
  has.pref <- start.df$found
  has.suf <- end.df$found
  has.only.fix <- has.pref & has.suf & names.len==1
  has.both <- has.pref & has.suf & names.len >=3
  has.pref.only <- has.pref & !has.suf & names.len >=2
  has.suf.only <- !has.pref & has.suf & names.len >=2
  
  names.l[has.only.fix] <- lapply(names.l[has.only.fix],function(name.comps){''})
  ## Have suffix and prefix
  names.l[has.both] <- lapply(names.l[has.both],function(name.comps){name.comps[2:(length(name.comps)-1)]})
  ## have prefix only
  names.l[has.pref.only] <- lapply(names.l[has.pref.only],function(name.comps){name.comps[-1]})
  ## have suffix only
  names.l[has.suf.only] <- lapply(names.l[has.suf.only],function(name.comps){name.comps[-length(name.comps)]})
  
  names.len[has.only.fix] <- 0
  names.len[has.both] <- names.len[has.both]-2
  names.len[has.pref.only | has.suf.only] <- names.len[has.pref.only | has.suf.only]-1
  
  sepname.df <- data.frame(clean.name=sapply(names.l,function(tt){paste(tt,collapse=' ')}),orig=name.v, name.len=names.len, stringsAsFactors=F)
  sepname.df$found.both <- has.both
  sepname.df$found.any <- start.df$found | end.df$found
  sepname.df$type <- 0
  sepname.df$str=sepname.df$categ <- ''
  sepname.df$type[has.both] <- 3
  sepname.df[has.only.fix | has.pref.only,c('str','categ','type')] <- start.df[has.only.fix | has.pref.only,c('psfix.str','psfix.categ','psfix.type')]
  sepname.df[has.suf.only,c('str','categ','type')] <- end.df[has.suf.only,c('psfix.str','psfix.categ','psfix.type')]
  
  if(any(has.both)){
    sepname.df <- cbind(sepname.df,start.df,end.df)
  }
  
  write(date(),file=paste('~/SNA/code/outputlogs/CleanPSfixLog.txt'),append=TRUE)
  write.table(data.frame(orig=name.v,clean=sepname.df$clean.name)[has.pref | has.suf,],file=paste('~/SNA/code/outputlogs/CleanPSfixLog.txt'),sep='\t',append=TRUE,row.names=FALSE)
  return(sepname.df)
}


### Given a vector of words wor.v, check if they are pre or suffix
GetPSfix <- function(word.v,prefix.df){
  
  ret.df <- data.frame(orig=word.v,psfix.categ='',psfix.type='',psfix.str='',found=FALSE,stringsAsFactors=F)
  
  ### mutually exclusive
  hasDot <- grep('\\.',word.v)
  isMatch <- which(word.v %in% unlist(prefix.df$str))
  
  ret.df$psfix.str[hasDot] <- sapply(strsplit(word.v[hasDot],split='.',fixed=T),function(s)s[1])
  ret.df$psfix.str[isMatch] <- word.v[isMatch]
  
  ret.df$psfix.categ[c(hasDot,isMatch)] <- prefix.df$categ[match(ret.df$psfix.str[c(hasDot,isMatch)],prefix.df$str)]
  ret.df$psfix.type[c(hasDot,isMatch)] <- prefix.df$type[match(ret.df$psfix.str[c(hasDot,isMatch)],prefix.df$str)]
  
  ret.df$found[c(hasDot,isMatch)] <- TRUE
  ret.df
}

## systematic name Expansion
## @ref.id= variable identifying unique individuals
## @namevars: anem componenets we want to permute
## @ref.id = SEARCHID or contactID to group duplicates (ie. same name and diff.id is not duplicate)
GetNamePermutations <- function(df,ref.id,namevars=c('name1','name2','name3','name4')){
  
  df$origName <- do.call('paste',c(df[namevars],sep=' '))
  
  ## combinations for each name length
  perm.df <- do.call('rbind',lapply(2:length(namevars),function(name.len){
    ### people with this name length
  
    name.cmps <- namevars[1:name.len]
    is.name.len <- df[[namevars[name.len]]] != '' 
    if(name.len < length(namevars)) is.name.len <- is.name.len & df[[namevars[name.len+1]]] == '' ### has name of this length
    combos <- gtools::permutations(name.len,name.len)  ## matrix: row=1 new combo, col=order name.cmps
    if(sum(is.name.len)==0) return(NULL)
    ret.df <- do.call('rbind',lapply(2:nrow(combos),function(comb.ind){
      comb <- combos[comb.ind,]
      comb.df <- df[is.name.len, ]
      comb.df[ name.cmps ] <- df[is.name.len, name.cmps[comb] ]
      comb.df$uniqueID <- paste(comb.df[[ref.id]],comb.ind,sep='_')
      return(comb.df)
      }))
    return(ret.df) 
  }))
  df$uniqueID <- df[[ref.id]]
  ret.df <- rbind(df,perm.df)
  
  ret.df <- ret.df[!duplicated(do.call('paste',c(ret.df[c(namevars,ref.id)],sep='.'))),]
  ret.df$refID <- ret.df[[ref.id]]
  return(ret.df)
}



## @df <- name.df with name1, name2,... for name to comapre, and columsn name_a1, name_a2, name_b1, name_b2,... to expad
## @nameoptions.list <- list(a=(name_a1,namea_2),b=c(name_b1,name_b2),..)
## @ orig.names: should have at least one more compoenent than all anmes
## @ref.id = SEARCHID or contactID to group duplicates (ie. same name and diff.id is not duplicate)
ExpandNamePermutations <- function(df,ref.id,orig.names=c('name1','name2','name3','name4'),
                                   nameoptions.list=list(a=c('name_a1','name_a2'),b=c('name_b1','name_b2'),c=c('name_c1','name_c2'),d=c('name_d1','name_d2')),permute=T){
  
  ### for each name ption a, b c, d
  new.df <- do.call('rbind',lapply(1:length(nameoptions.list),function(ind){
    name.cmps <- nameoptions.list[[ind]]
    
    has.name.cmps <- do.call('paste',c(df[name.cmps],sep='.')) != '.'  ## any of the name.cmps is not ''   ## non-empty option
    diff.name.cmps <- (do.call('paste',c(df[name.cmps],sep='')) != do.call('paste',c(df[orig.names],sep='')))  ## name_abcd, not same has name1, name2,,
    change.df <- df[has.name.cmps & diff.name.cmps,]  ### new data fraome only for rows with new name optionss
    change.df[orig.names[1:length(name.cmps)]]  <- change.df[name.cmps]   ### anme1 anme1 into name optoin.  
    change.df[orig.names[-(1:length(name.cmps))]] <- ''  
    change.df$uniqueID <- paste(change.df$uniqueID,ind,sep='_')
   
    nofamilyname <- change.df$familyname=='DONT KNOW'
    if(any(nofamilyname)){
     perm.df <- change.df[nofamilyname,]   ##
     perm.df[[orig.names[1] ]] <- perm.df[[orig.names[2] ]]
     perm.df[[orig.names[2] ]] <- change.df[[ orig.names[1] ]][nofamilyname]
     perm.df$uniqueID <- paste(perm.df$uniqueID,'p',sep='_')
     change.df <- rbind(change.df,perm.df)
    }
    return(change.df)
  }))
  
  ret.df <- rbind(df,new.df)
 
  
  if(permute){
    perm.df <- ret.df
    perm.df[[orig.names[1] ]] <- perm.df[[orig.names[2] ]]
    perm.df[[orig.names[2] ]] <- ret.df[[ orig.names[1] ]]
    perm.df$uniqueID <- paste(perm.df$uniqueID,'p',sep='_')
    ret.df <- rbind(ret.df,perm.df)
  }
  ret.df <- ret.df[!duplicated(do.call('paste',c(ret.df[c(orig.names,ref.id)],sep='.'))),]
  ret.df$refID <- ret.df[[ref.id]]
  return(ret.df)
  
}

has.sim <- function(check.vect, names.vect,str.bound){
  sapply(check.vect,function(xx){any(jarowinkler(xx,names.vect) >= str.bound)})
}
CorrectFamilyName <- function(p.df,search.df){
  need.change <- p.df$familyname=='DONT KNOW'
  p.df$familyname[need.change] <- toupper(search.df$last[match(p.df$SEARCHID[need.change], search.df$SEARCHID)])
  
  ## for those with name1 different from family name
  p.df <- Correct3NameComponent(p.df)
  
  switched <- need.change & p.df$familyname != p.df$name1
  
   pos <- data.frame(lapply(c(name2='name2',name3='name3',name4='name4'),function(pos){
    ret <- p.df[[pos]]
    in.here <- switched & need.change & (p.df$familyname == p.df[[pos]])
    ret[in.here] <- p.df$name1[in.here]
    return(ret)
  }),stringsAsFactors=F)
  p.df[c(name2='name2',name3='name3',name4='name4')] <- pos
  p.df$name1[switched & need.change] <- p.df$familyname[switched & need.change]
  return(p.df)
}
TurnFactorsToChar <- function(df){
  print(names(df)[sapply(df,is.factor)])
  df[sapply(df,is.factor)] <- data.frame(lapply(df[sapply(df,is.factor)],as.character),stringsAsFactors = F)
  return(df)
}
Correct3NameComponent <- function(df){
  switched <- df$familyname==df$name3 & df$familyname!=''
  df[switched,c('name2','name3')] <- df[switched,c('name1','name2')]
  df$name1[switched] <- df$familyname[switched]
  return(df)
}
## combines new names using otheranme, and name components
## first incorporate other naem, then parenthename
IncorporateOthername <- function(df,other.var,remnants.var){
    
    df$do.var <- df[[other.var]]
    othername.df <- df
    othername.df <- othername.df[setdiff(names(othername.df),c(c(paste('name',1:3,sep=''),'othername.clean')))]
    othername.df <- BreakName(df = othername.df,name.var = 'do.var',cmps.n = 3)[c(paste('name',1:3,sep=''),'othername.clean')]
    
    ## fill name components in order
    for(oo in 1:3){
      other.cmp <- paste('name',oo,sep='')
      other.nonempty <- othername.df[[other.cmp]]!=''
      ## check if word already in comps
      in.cmps <- rep(TRUE,nrow(othername.df))
      
      if(any(other.nonempty)){
      in.cmps[other.nonempty] <- sapply(which(other.nonempty),function(xx)othername.df[[other.cmp]][xx] %in% df[xx,paste('name',1:4,sep='')])
      othername.df[[other.cmp]][in.cmps] <- ''
      }
      has.other <- othername.df[[other.cmp]]!=''
      xx <- 2
       while(any(has.other) & xx <=4){
         ## updated every time other.cmp is filled
        name.cmp <- paste('name',xx,sep='')
        name.empty <- df[[name.cmp]]==''
        
        ## if use this name. comps
        if(xx==2) use.name <- has.other & name.empty
        else{
          previous.empty <- df[[paste('name',xx-1,sep='')]] ==''
          use.name <- has.other & !previous.empty & name.empty
        }
        df[use.name,name.cmp] <- othername.df[use.name,other.cmp]
        othername.df[use.name,other.cmp] <- ''  ## if moved to a component then delete
        has.other <- othername.df[[other.cmp]]!=''
        xx <- xx+1
      }
    }
    
    ## othercmps that didn't get filled are put in a reservoice
    if(!remnants.var %in% names(df)) df[[remnants.var]] <- ''
    df[df$do.var!='',remnants.var] <- gsub("^ *|(?<= ) | *$", "", do.call('paste',c(cbind(df[df$do.var!='',remnants.var],othername.df[df$do.var!='',]),sep=' ')) ,perl=T)
    df$do.var <- NULL
  return(df)  
}
## make sure name 1 does not have a given name
## search.last= same order as df
AnchorName1 <- function(df, name.vars=c('name1','name2','name3','name4'),search.last, given.names,str.bound=0.95){
  is.given <- data.frame(lapply(df[name.vars],function(xx) has.sim(xx, given.names,str.bound)))
  
  ## which component is non.given
  non.given <- data.frame(lapply(c('name2','name3','name4'),function(comp){
    
    ## this component is non.given
    ret <- rep(100,nrow(df))
    ret[!is.given[[comp]] & df[[comp]]!=''] <- switch(comp,name2=2,name3=3,name4=4)
    return(ret)
  }))
  first.non.given <- apply(non.given,1,min) ## first non.given position that is free.
  
  ## if first is given and has non.given comps, switch
  for(comp.ind in 2:4){
    start.here <- first.non.given==comp.ind  ## all with non.given
    use.comp <- name.vars[comp.ind]
    temp <- df[start.here & is.given$name1,use.comp] 
    df[start.here & is.given$name1 ,use.comp] <- df[start.here & is.given$name1,'name1']
    df[start.here & is.given$name1,'name1'] <- temp
  }
  
  ## if first is given and has all.given.comps, default back to search.last
  all.given <- rowSums(non.given)==300 & is.given$name1
  not.last <- all.given & (df$name1 != search.last)
  pos <- data.frame(lapply(c(name2='name2',name3='name3',name4='name4'),function(pos){
    ret <- df[[pos]]
    in.here <- not.last & (df[[pos]]==search.last)
    ret[in.here] <- df$name1[in.here]
    return(ret)
  }),stringsAsFactors=F)
  df[c(name2='name2',name3='name3',name4='name4')] <- pos
  df$name1[not.last] <- search.last[not.last]
  return(df)
}



## get the highest weighted pairs for a given link. one contact can be linked to many participants with one linke type, 
### should used weights to discern the matches
## @reclink.df is a 'RecLinkData' or RecLinkResult object
## @ link.type ='L' for link, 'N' ofr non-link, 'P' for possible link
## @contactID=column of hte ids that have to be uniquely matched.
## attOrder=order in which the compared attributes are returned. 
getLinkedPairs <- function(reclink.df, attOrder, id.vars=c('uniqueID','refID','origName'),link.type='L',threshold, partiID, contactID,
                           dedupContact=TRUE, weighted=TRUE,age.bound=NULL,strThreshold=NULL,components=c('sim.name1','sim.name2','sim.name3'),simple=FALSE){
  
  #if(class(reclink.df)!='RecLinkData') stop('reclink.df has to be a RecLinkData object')
  w8.pairs <- getPairs(reclink.df, single.rows=TRUE,sort=FALSE)
  w8.pairs <- cbind(w8.pairs,sim=reclink.df$pairs[intersect(attOrder,names(reclink.df$pairs))])
  
  w8.pairs$link <- 1
  
  if(class(reclink.df)=='RecLinkResult'){
  w8.pairs <- w8.pairs[as.character(reclink.df$prediction)==link.type,]
  w8.pairs$link <- link.type
  }
  else{
    w8.pairs <- w8.pairs[w8.pairs$Weight>=threshold,]
    
  }
  
  if(!is.null(age.bound)){w8.pairs <- w8.pairs[!is.na(w8.pairs$age.2) & abs(w8.pairs$age.1-w8.pairs$age.2) <= age.bound ,]}
  
  if(!weighted) return(w8.pairs)
  ## after ordering by decreasing weight, take the first occurence
  w8.pairs <- w8.pairs[order(w8.pairs$Weight,decreasing=T),]
  
  ## take out duplicates of the same pair that are due to permulations
  dup.perm <- duplicated(do.call('paste',c(w8.pairs[c(partiID,contactID)],sep='.')))
  w8.pairs <- w8.pairs[!dup.perm,]
  if(simple)return(list(pre.fp=w8.pairs))
  
  f.p <- rep(FALSE, nrow(w8.pairs))
  if(!is.null(strThreshold)){
    if(length(strThreshold)==1){strThreshold <- rep(strThreshold, length(components))}
    if(is.null(components))stop('compoenents missing')
    f.p <- rowSums(data.frame(lapply(1:length(components),function(ind){
      comp <- w8.pairs[components[ind]]
      as.numeric(!is.na(comp) & comp< strThreshold[ind])
    }))) > 0  ## any component < threhsold
  }
  
  make.ret <- function(df,dedupContact){
    dup <- rep(FALSE,nrow(w8.pairs))
    if(dedupContact) dup <- duplicated(df[[contactID]])
    return(df[!dup ,c('id1','id2', paste(rep(setdiff(attOrder,id.vars),each=2),rep(1:2,length(setdiff(attOrder,id.vars))),sep='.'),
                      paste('sim',setdiff(attOrder,id.vars),sep='.'),
                      paste(rep(id.vars,each=2),rep(1:2,length(id.vars)),sep='.'),
                      'Weight','link')])
  }
  
  ret.list <- list(dupContact=make.ret(w8.pairs,FALSE), pre.fp=make.ret(w8.pairs,dedupContact))
  ret.list$post.fp <- NULL
  if(any(f.p)){
    ret.list$post.fp=make.ret(w8.pairs[!f.p,],dedupContact)
  }
  return(ret.list)
}

GetWeightedData <- function(attr.w8,linkdata){
  error.rate <- GetErrorRates(attr.w8,freq = linkdata$frequencies,start.df=data.frame(0.01+.01*diag(length(attr.w8))))
  epiw8 <- epiWeights(rpairs=linkdata,e=as.numeric(error.rate[1,(1:length(attr.w8))]))
  return(list(error.rate=error.rate,weight.data=epiw8))
}

GetMatchedData <- function(weighted.data,w8.quantile=NULL,threshold=NULL, link.type='L',partiID, contactID,attOrder,age.bound=10, 
                           strThreshold,components=c('sim.name1','sim.name2','sim.name3'),useClassify=F){
  
  if(is.null(threshold)){
  gpd.lower <- quantile(weighted.data$weight.data$Wdata,probs=w8.quantile)
  threshold <- gpdEst(weighted.data$weight.data$Wdata,thresh=gpd.lower)
  }
  
  use.data <- weighted.data$weight.data
  if(useClassify){use.data <- epiClassify(weighted.data$weight.data,threshold.upper=threshold)}
  
  linked.pairs <- getLinkedPairs(use.data,link.type='L',threshold=threshold, partiID=partiID, contactID = contactID,
                                 attOrder=attOrder,age.bound=age.bound, strThreshold=strThreshold, components=components)
  
  return(list(error.rate=weighted.data$error.rate,w8.quantile=w8.quantile,threshold=threshold,
              linked.pairs.dupContact=linked.pairs$dupContact, linked.pairs.dedupContact=linked.pairs$pre.fp, linked.pairs.postfp=linked.pairs$post.fp))
}


## return subset of df that satisfies bound for variable
BoundVar <- function(df, var1,var2, bound){
  df[abs(df[[var1]]-df[[var2]])<bound,]
}

## @start.df: rows=dimension of attr.w8, cols=start points to try
GetErrorRates <- function(attr.w8, freq, start.df,algo='NLOPT_GN_ISRES',loss.f=sum){
  if(length(attr.w8)!=length(freq)) stop('different length of attr.w8 and freq')
  
  attr.ind <- 1:length(attr.w8)
  
  attr.w8.system <- function(err){
    w8 <- log((1-err)/freq, base=2)
    foos <- sapply(attr.ind,function(ind){
      (w8[ind]/(sum(w8)))
    })
  }
  
  loss <- function(err){
    loss.f( (attr.w8.system(err)-attr.w8)^2)
  }
  
  ret.df <- data.frame(t(data.frame(lapply(start.df,function(start){
   # browser()
#     ss <- nloptr(x0=start,eval_f=loss,lb=rep(0,length(freq)),ub=1-freq,opt=list(algorithm=algo))
#     return(c(error.rate=ss$solution,f.value=ss$objective,attrw8=attr.w8.system(ss$solution)))
#         ss <- spg(par=start,fn=loss,lower=rep(0,length(freq)),upper=1-freq)
#          return(c(error.rate=ss$par,f.value=ss$value,attrw8=attr.w8.system(ss$par)))
    ss <- gosolnp(fun=loss,LB=rep(0,length(freq)),UB=1-freq,control=list(trace=0))
    return(c(error.rate=ss$pars,f.value=ss$values[length(ss$values)],attrw8=attr.w8.system(ss$pars)))
    }))))
  
  return(ret.df[order(ret.df$f.value),])
}
randomRows = function(df,n){
  if(is.null(n)){stop('given n')}
  return(df[sample(nrow(df),n),])
}

AssessLoss <- function(linked.pairs,compThreshold,components=c('sim.name1','sim.name2','sim.name3'), expect.nMatch){
  if(length(compThreshold)==1){compThreshold <- rep(compThreshold, length(components))}
  f.p <- rowSums(data.frame(lapply(1:length(components),function(ind){
    comp <- linked.pairs[components[ind]]
    as.numeric(!is.na(comp) & comp< compThreshold[ind])
    }))) > 0  ## any component < threhsold
  ret.list <- list(FPrate=sum(f.p)/nrow(linked.pairs),matched=(nrow(linked.pairs)-sum(f.p))/expect.nMatch,FP=f.p)
  ret.list$cost <- ret.list$FPrate/ret.list$matched
  return(ret.list)
}
CheckBadName <- function(linked.pairs,compThreshold,components=c('sim.name1','sim.name2','sim.name3')){
  rowMeans(link.pairs[components],na.rm=T) <= compThreshold
}

## @weight.len = # different weights, shsould sum to 1
## sample param so that w1 >= all tohers, w.len<= all others
MakeParamSpace <- function(nsamp, weight.len=5, quant.range){
  
  round.simplex <- function(df, signif=2){
    ret <- round(df[,-ncol(df)],signif)
    ret <- cbind(ret,1-rowSums(ret))
  }
  use.point <- function(df){
    use <- df[,1]==apply(df,1,max) & df[,ncol(df)]==apply(df,1,min)
  }
  
  w8.df <- simplex.sample(n = weight.len,nsamp)$samples
  use <- use.point(w8.df)
  while(!all(use)){
    w8.df[!use,] <- simplex.sample(n = weight.len,sum(!use))$samples
    dup <- duplicated(do.call('paste',c(as.data.frame(w8.df),sep='_')))  ## check no duplicate weights
    use <- !dup & use.point(w8.df)
  }
  
  param.df <- do.call('rbind',lapply(seq(from=quant.range[1],to=quant.range[2],by=.01),function(ind){cbind(w8.df,ind)}))
  
}

### @attOrder: first 6 are main fields, last 4 are equally  unimportnat
### @param.space = c(attr.w8.5, w8.quantile)= cols, rows=point on paramspace
OptimMatchParam <- function(link.data, param.space, strThreshold=NULL, weight.split.ind,
                            partiID='refID.1',contactID='refID.2',age.bound=10,attOrder=NULL){
  
  ## 5 weights fpr foe;ds: n1, n2, n3, age, village,sex (n4, other, suffix, honorific), deg. freedom=4, attr.w8.5=length4 for first 4 weights
  ## 1 quantile
  PerformMatch <- function(param.ind){
    param <- as.numeric(param.space[param.ind,])
   
    attr.w8.5 <-  param[-length(param)]
    nsplits <- length(link.data$frequencies)-(weight.split.ind) + 1
    w8.quantile <- param[length(param)]
    attr.w8 <- c(attr.w8.5[1:(weight.split.ind-1)], rep(attr.w8.5[weight.split.ind]/nsplits,nsplits))
    
    cat(date(), param.ind,' getting weights\n')
    weighted.data <- GetWeightedData(attr.w8 = attr.w8, linkdata = link.data)
    if(is.null(attOrder)) attOrder <- names(link.data$frequencies)
    cat(date(), param.ind,'matching\n')
    matched.data <- GetMatchedData(weighted.data, w8.quantile = w8.quantile,partiID=partiID, contactID = contactID, attOrder = attOrder, age.bound = age.bound, strThreshold=strThreshold)
    
    return(matched.data)
  } 
  
  if(!RUNPARALLEL){
    match.l <- lapply(1:nrow(param.space), PerformMatch)
  }
  else{
    match.l <- mclapply(1:nrow(param.space), PerformMatch,mc.cores=ncl)
  }
  return(match.l)
}