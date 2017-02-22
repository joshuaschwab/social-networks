library(igraph)
library(gtools)
library(tabplot)
library(parallel)
library(xlsx)
library(Hmisc)
library(ggplot2)
library(rgexf)
library(stringdist)
library('RecordLinkage')

source('~/Desktop/search_network_code/MakeNetwork_methods.R')
source('~/Dropbox/Network/fall2016_prematch_code/CleanNames_methods.R')

################################################################
load("/Users/yiqunc/Dropbox/Network/10_10_final_pairs/final_fixed_wz_protocol/fixed_final_pairs_kisegi.RData")
parish <- 'kisegi' #'kamuge'

load('~/Dropbox/network_yiqun/fall2016_prematch/permutation_fix/myPrematch_kisegi.RData')


#load('~/Dropbox/Network/10_10_final_pairs/09_28_new_')

# load raw data sheet
load(paste0('~/Dropbox/Network/10_10_final_pairs/09_28_new_',parish,'_name_fixation.RData'))
#save(combined_linked_pairs,file='~/Dropbox/Network/10_10_final_pairs/permutation_fixed/combined_final_pairs_kisegi.RData')

#SNA_new_dataset_kisegi$tracking_id
#
# load fixed  final pairs

#load(paste0('~/Dropbox/Network/10_10_final_pairs/final_fixed_wz_protocol/fixed_final_pairs_',parish,'.RData'))

link.pairs <- combined_linked_pairs.fixed
##################################c##############################
### using my c2p matches and c2c matches
################################################################
# this is the pc + pc remains pairs all the pc matches


link.pairs.cp <- subset(link.pairs,link==1)

rm(link.pairs)
# villages.study 
villages.study = unique(toupper(sort(unique(SNA_new_dataset_kisegi$village_name))))
villages.study = villages.study[villages.study!=""]

villages.contacts <- gsub('^ *|(?<= ) | *$', "",perl=T,sort(unique(prematch.c$village)))
villages.contacts = villages.contacts[villages.contacts!=""]

villages.correct <- GetVillagesCorrect(villages.study,contacts.v = villages.contacts,method = c('jw','osa'),cutoff = .23,p=0)


table(link.pairs.cp$village.2 %in% villages.study)

# not doing cc for now
# load(paste('outputs/myMatches_method2_Matched_',parish,'_','CCremains','.RData',sep=''))
# link.pairs.cc <- subset(link.pairs,link==1)
# rm(link.pairs)

# fine till now; all our pairs are in the orig data
myPairs <- getMyPairs(link.pairs.cp,cc.df = NULL)

# not doing cc for now
#myPairs <- ManualFixMyPairs(GetCtoCNodes(pairs.df = myPairs,myPairs, info.longc = prematch.c),pairs.df = myPairs, prematch.c,prematch.p,parish)

#save(myPairs,file=paste('~/Dropbox/Network/2017_final_network/myMatches_method2_Pairs_',parish,'.RData',sep=''))
################################################################
## nodes = {participants (unmatched parti + matched parti-cont)} + {unmatchedToPartciCont (unmatched to other cont + matched to other contact)}

#load(file=paste('~/Dropbox/network_yiqun/fall2016_prematch/no_perm_myPrematch_kisegi.RData'))
#pre_permutation.pematch.p

# deduplicate to get unique nodes
pre_permutation.pematch.p <- prematch.p[!duplicated(prematch.p$SEARCHID),]
pre_permutation.pematch.p <- pre_permutation.pematch.p[!is.na(pre_permutation.pematch.p$SEARCHID),]
#;D
p.nodeInfo <- GetPartiNodes(ids.df = pre_permutation.pematch.p,info.p.dfs = list(census=SNA_new_dataset_kisegi))  #raw.track





unmatched.c <- GetUnmatchedContactsNodes(pairs.df = myPairs, info.longc = prematch.c)
#c2c <- GetCtoCNodes(pairs.df = myPairs,info.longc = prematch.c)
#c2c.reps <- data.frame(table(myPairs$uniqueID[!(myPairs$uniqueID %in% prematch.p$SEARCHID)]))
#summary(c2c.reps$Freq)  ## use freq=3, i.e. 3rd quantile.



for (col in colnames(p.nodeInfo)){
  p.nodeInfo[,col] <- unlist(p.nodeInfo[,col])
}


nodeInfo <- MakeAllNodes(p.nodeInfo, c2c = NULL, un.c.nodeInfo = unmatched.c, studyVillage = villages.correct$orig)

# make sure it's one
nodeInfo$rep <- 1




#nodeInfo$rep[nodeInfo$type=='c2c'] <- c2c.reps$Freq[match(nodeInfo$uniqueID[nodeInfo$type=='c2c'],c2c.reps[[1]])]
edges <- MakeEdges(myPairs,unmatched.c,prematch.c)## duplicated edges when a participant names the same person more than once.
edges$endp <- edges$uniqueID %in% prematch.p$SEARCHID
edges <- edges[edges$namerSID!=edges$uniqueID,]

real.nodeInfo <- nodeInfo
stable_data_only <- SNA_new_dataset_kisegi[,c('tracking_id','newstable')]
stable_data_only$newstable = as.numeric(stable_data_only$newstable=='stable')

# stable from study 
real.nodeInfo <- merge( real.nodeInfo, stable_data_only,by.x='uniqueID',by.y='tracking_id',all.x=T,all.y=F)

# stable age
real.nodeInfo$stable.adult <- (real.nodeInfo$age >= 15)&(real.nodeInfo$newstable)
#nrow(real.nodeInfo)

row.names(real.nodeInfo) <- real.nodeInfo$uniqueID
real.edges <- edges
real.edges$snTypes=real.edges$src <- NULL
edges$snTypes=edges$src <- NULL
edges$is.samehh <- gsub( "-.*$", "", edges$namerSID) == gsub( "-.*$", "", edges$uniqueID)
####################################################################################
# we just pick the ones that are stable

stable.real.nodeInfo <- real.nodeInfo[real.nodeInfo$newstable==1,]
stable.real.nodeInfo <- stable.real.nodeInfo[!is.na(stable.real.nodeInfo$newstable),]

stable.edges <- edges[edges$uniqueID %in% stable.real.nodeInfo$uniqueID & edges$namerSID %in% stable.real.nodeInfo$uniqueID,]
stable.edges.nohh <- subset(stable.edges, !is.samehh)
####################################################################################

##################### network and gc
snType <- c('free','emot','food','money','health')
snInd <- paste('is',snType,sep='.')
names(snInd) <- snType


snColor <- c('red','green','blue','cyan','magenta')
names(snColor) <- snType

ug <- Makeigraph(d=real.edges,vertices=real.nodeInfo, directed=F)
for(att in snInd){
  value <- get.edge.attribute(ug,att)>0
  ug=remove.edge.attribute(ug,att)
  ug=set.edge.attribute(ug,name=att,value=value)
}
dg <- Makeigraph(d=real.edges,vertices=real.nodeInfo, directed=T)

## clusters
ug.clus <- igraph::clusters(ug)
table(ug.clus$csize)
#xtable(t(cbind(data.frame(table(ug.clus$csize)))))
ug.gc <- decompose.graph(ug)[[1]]

dg.clus <- igraph::clusters(dg,mode='weak')
table(dg.clus$csize)
#xtable(t(cbind(data.frame(table(dg.clus$csize)))))
dg.gc <- decompose.graph(dg)[[1]]

ug.gc.subs <- c(all=list(ug.gc),lapply(snInd,function(ind){MakeSubNetwork(ug.gc,ind)}))
dg.gc.subs <- c(all=list(dg.gc),lapply(snInd,function(ind){MakeSubNetwork(dg.gc,ind)}))
ug.subs <- c(all=list(ug),lapply(snInd,function(ind){MakeSubNetwork(ug,ind)}))
dg.subs <- c(all=list(dg),lapply(snInd,function(ind){MakeSubNetwork(dg,ind)}))
# dg.gc.pairsubs <- MakePairsSubNetwork(dg.gc,snInd)

### local networks
ug.subs <- lapply(ug.subs,function(gg){
  set.vertex.attribute(gg,name = 'stable',value = real.nodeInfo[V(gg)$name,'stable.adult'])})
dg.subs <- lapply(dg.subs,function(gg){
  set.vertex.attribute(gg,name = 'stable',value = real.nodeInfo[V(gg)$name,'stable.adult'])})


# run it until here for now 0405; send the attachment if finished 

deg_ug <- degree(ug, mode="all")

getwd()
setwd('~/Dropbox/Final_Networks/Data/')
save.image(file =paste0(parish,'_current_network.RData'))
#load('ogongo_current_network.RData')

#edges <- MakeEdges(myPairs,unmatched.c,prematch.c)
summary(ug)
transitivity(ug,type='undirected') 
mean(deg_ug)
reciprocity(dg)


load('./nyatoto_current_network.RData')

ug_stable <- Makeigraph(d=stable.edges,vertices=stable.real.nodeInfo, directed=F)
for(att in snInd){
  value <- get.edge.attribute(ug,att)>0
  ug=remove.edge.attribute(ug,att)
  ug=set.edge.attribute(ug,name=att,value=value)
}
dg_stable <- Makeigraph(d=stable.edges,vertices=stable.real.nodeInfo, directed=T)


summary(ug_stable)
transitivity(ug_stable,type='undirected') 
deg_ug_stable <- degree(ug_stable, mode="all")

mean(deg_ug_stable)
reciprocity(dg_stable)
distMatrix <- shortest.paths(ug_stable, v=V(ug_stable), to=V(ug_stable))

distMatrix[!is.finite(distMatrix)] <- NA
avg_distMatrix <- rowMeans(distMatrix,na.rm=TRUE)
summary(avg_distMatrix)
#colMeans(m, )

save(avg_distMatrix,ug_stable,dg_stable,stable.edges,stable.real.nodeInfo, parish,file =paste0(parish,'_stable_only_network.RData'))








