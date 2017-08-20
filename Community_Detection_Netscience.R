############################################################################################
############################# R Program for Community Detection ############################
############################################################################################

##### Defining the libraries used ##############################
library(igraph)
library(dplyr)
library(magrittr)

####### Reading the Data from the gml file ######################################
####### Need to make changes in the path as required ############################
G<-read.graph("F:/KE_CA_WebAnalytics/netscience.gml",format=c("gml"))

####### Summary of igraph object and plot #######################################
summary(G)
plot(G)

######## Total Number of Cliques in the graph ###################################
clique.number(G)


memberships <- list()

#########################################################################
################### edge.betweenness.community Detection ###########################
#########################################################################
ebc <- edge.betweenness.community(G)
mods <- sapply(0:ecount(G), function(i) {
  g2 <- delete.edges(G, ebc$removed.edges[seq(length=i)])
  cl <- clusters(g2)$membership
  modularity(G, cl)
})

g2 <- delete.edges(G, ebc$removed.edges[1:(which.max(mods)-1)])
memberships$`Edge betweenness` <- clusters(g2)$membership
modularity(G,memberships$`Edge betweenness`)
membership(ebc)
sizes(ebc)
plot(ebc,G)
############ To get the Number of members in the top Clusters of graph #################
topClusters_ebc <- table(ebc$membership) %>% sort(decreasing=TRUE) %>% head(10)
topClusters_ebc[1:10]
plot(topClusters_ebc, main="Cluster size", ylab="Number of members", type="b", lwd=2)


#########################################################################
################### fastgreedy.community Detection #################################
#########################################################################

fc <- fastgreedy.community(G)
memberships$`Fast greedy` <- cutat(fc,steps = which.max(fc$modularity)-1)
modularity(G,memberships$`Fast greedy`)
membership(fc)
sizes(fc)
plot(fc,G)
dendPlot(fc,mode="hclust")
############ To get the Number of members in the top Clusters of graph #################
topClusters_fc <- table(fc$membership) %>% sort(decreasing=TRUE) %>% head(10)
topClusters_fc[1:10]
plot(topClusters_fc, main="Cluster size", ylab="Number of members", type="b", lwd=2)

############################################################################################
################## leading.eigenvector.community Detection ############################
############################################################################################
lec <- leading.eigenvector.community(G)
memberships$`Leading eigenvector` <- lec$membership
modularity(G,memberships$`Leading eigenvector`)
membership(lec)
sizes(lec)
plot(lec,G)
############ To get the Number of members in the top Clusters of graph #################
topClusters_lec <- table(lec$membership) %>% sort(decreasing=TRUE) %>% head(10)
topClusters_lec[1:10]
plot(topClusters_lec, main="Cluster size", ylab="Number of members", type="b", lwd=2)


############################################################################################
############### Infomap Method Community Detection ####################################
############################################################################################

imc <- infomap.community(G)
memberships$`Infomap Method` <- imc$membership
modularity(G,memberships$`Infomap Method`)
membership(imc)
sizes(imc)
plot(imc,G)
############ To get the Number of members in the top Clusters of graph #################
topClusters_imc <- table(imc$membership) %>% sort(decreasing=TRUE) %>% head(10)
topClusters_imc[1:10]
plot(topClusters_imc, main="Cluster size", ylab="Number of members", type="b", lwd=2)


############################################################################################
############## walktrap.community Detection ############################################
############################################################################################

wt <- walktrap.community(G, modularity=TRUE)
memberships$`Walktrap` <- cutat(wt,steps = which.max(wt$modularity)-1)
modularity(G,memberships$`Walktrap`)
membership(wt)
sizes(wt)
plot(wt,G)
############ To get the Number of members in the top Clusters of graph #################
topClusters_wt <- table(wt$membership) %>% sort(decreasing=TRUE) %>% head(10)
topClusters_wt[1:10]
plot(topClusters_wt, main="Cluster size", ylab="Number of members", type="b", lwd=2)

############################################################################################
################ label.propagation.community Detection #########################################
############################################################################################

lab <- label.propagation.community(G)
memberships$`Label propagation` <- label.propagation.community(G)
modularity(G,memberships$`Label propagation`)
membership(lab)
sizes(lab)
plot(lab,G)
############ To get the Number of members in the top Clusters of graph #################
topClusters_lab <- table(lab$membership) %>% sort(decreasing=TRUE) %>% head(10)
topClusters_lab[1:10]
plot(topClusters_wt, main="Cluster size", ylab="Number of members", type="b", lwd=2)

############################################################################################
########################### K-Core Decomposition ##################################
############################################################################################

kc <- coreness(G, mode="all")

plot(G, vertex.size=kc*6, vertex.label=kc, vertex.color=colrs[kc])
