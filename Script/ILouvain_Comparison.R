
library(devtools)
load_all()
library(igraph)
library(reshape2)
library(ggplot2)



Path = "/home/issam/Dropbox/"
PathData= paste0(Path,"ILouvain/Graphes/graphes/test_")

mainDir <- paste(Path,"Benchmark/Thesis/Attributed Network Clustering/ILouvain/",sep="")
subDir <-  paste(Sys.Date(),"__2","/",sep="")
dir.create(file.path(mainDir,subDir), showWarnings = FALSE)
output <- paste(mainDir,"/",subDir,sep="")
dir.create(file.path(output,"Partitions"), showWarnings = FALSE)
dir.create(file.path(output,"Figures"), showWarnings = FALSE)
file <- paste(output,"results.txt",sep="")

PathFigureSaving = paste0(output,"Figures/")
GraphName = "Synthetic"



#Script to compare with ILouvain Algorithm

resARI = list()
resNMI = list()
for( iter in 1:100){
  print('*********************************************************************************************')
  print(iter)
  ARI = vector()
  NMI = vector()
  graph = graph.data.frame(read.csv2(paste0(PathData,iter,".edgeList"),header = FALSE,sep = " ",blank.lines.skip = TRUE),directed = FALSE)

  # Add attributes
  tmp = read.csv2(paste0(PathData,iter,".attributes"),header = FALSE,sep = " ")
  tmp2 = do.call(rbind,strsplit(as.character(tmp[[2]]),split = ","))
  graph = set.vertex.attribute(graph,name = "name",value = as.character(tmp[,1]))
  graph = set.vertex.attribute(graph,name = "attr1",value = as.numeric(tmp2[,1])[1:vcount(graph)])
  graph = set.vertex.attribute(graph,name = "attr2",value =as.numeric(tmp2[,2]))

  tmp = read.csv2(paste0(PathData,iter,".community"),header = FALSE,sep = " ")
  real = tmp[[2]]
  names(real) = tmp[[1]]

  tmp = read.csv2(paste0(PathData,iter,".2ModLouvain"),header = FALSE,sep = " ")
  ModLouvain = tmp[[2]]
  names(ModLouvain) = tmp[[1]]

  k = length(unique(real))
  attr = getAll.attribute(graph)


  wtKmeans = kmeans(attr, k)$cluster
  wtLouvain = membership(cluster_louvain(graph))
  wtWalktrap = membership(cluster_walktrap(graph))

  ARI = c( ARI,round(compare(wtKmeans,real,method = "adjusted.rand"),4))
  ARI = c( ARI,round(compare(wtLouvain,real,method = "adjusted.rand"),4))
  ARI = c( ARI, round(compare(wtWalktrap,real,method = "adjusted.rand"),4))

  NMI = c( NMI,round(compare(wtKmeans,real,method = "nmi"),4))
  NMI = c( NMI,round(compare(wtLouvain,real,method = "nmi"),4))
  NMI = c( NMI, round(compare(wtWalktrap,real,method = "nmi"),4))

  wt = ANCA(graph,nbCluster = k,nbEigenVectorView=2, seedsFunction = DetectSeeds_Bicomponents,contextSimilarityFunction=dist,verbose = FALSE)
  ARI = c( ARI,round(compare(wt,real,method = "adjusted.rand"),4))
  NMI = c( NMI,round(compare(wt,real,method = "nmi"),4))

  wt = ANCA(graph,nbCluster =k,nbEigenVectorView=2, seedsFunction = DetectSeeds_Bicomponents,contextSimilarityFunction=dist,structuralSimilarityFunction = similarity.jaccard ,structuralIsSimilarity = TRUE,verbose = FALSE)
  ARI = c( ARI, round(compare(wt,real,method = "adjusted.rand"),4))
  NMI = c( NMI, round(compare(wt,real,method = "nmi"),4))

  wt = ANCA(graph,nbCluster =k,nbEigenVectorView=2, seedsFunction = DetectSeeds_Centrality,contextSimilarityFunction=dist,verbose = FALSE)
  ARI = c( ARI, round(compare(wt,real,method = "adjusted.rand"),4))
  NMI = c( NMI, round(compare(wt,real,method = "nmi"),4))

  wt = ANCA(graph,nbCluster =k, nbEigenVectorView=2, seedsFunction = DetectSeeds_Centrality,contextSimilarityFunction=dist,structuralSimilarityFunction = similarity.jaccard ,structuralIsSimilarity = TRUE,verbose = FALSE)
  ARI = c( ARI, round(compare(wt,real,method = "adjusted.rand"),4))
  NMI = c( NMI, round(compare(wt,real,method = "nmi"),4))


  wt = ANCA(graph,nbCluster =k,nbEigenVectorView=2, seedsFunction = DetectSeeds_Centrality_leaders,contextSimilarityFunction=dist,verbose = FALSE)
  ARI = c( ARI,  round(compare(wt,real,method = "adjusted.rand"),4))
  NMI = c( NMI,  round(compare(wt,real,method = "nmi"),4))

  wt = ANCA(graph,nbCluster =k,nbEigenVectorView=2, seedsFunction = DetectSeeds_Centrality_leaders,contextSimilarityFunction=dist,structuralSimilarityFunction = similarity.jaccard,structuralIsSimilarity = TRUE,verbose = FALSE )
  ARI = c( ARI,  round(compare(wt,real,method = "adjusted.rand"),4))
  NMI = c( NMI,  round(compare(wt,real,method = "nmi"),4))


  ARI = c( ARI,round(compare(ModLouvain,real,method = "adjusted.rand"),4))
  NMI = c( NMI,round(compare(ModLouvain,real,method = "nmi"),4))



  resARI = c(resARI, list(ARI))
  resNMI = c(resNMI, list(NMI))

}

matARI = do.call(rbind, resARI)
colnames(matARI) =  c("kmeans","Louvain","Walktrap","BiCC + SPath","BiCC + SimJa","Central + SPath","Central + SimJa", "LeaderC + SPath","LeaderC + SimJa", "ILouvain")
write.table(matARI, file = paste0(output,"Partitions/","ILouvainARI",".csv"),row.names = TRUE, col.names=TRUE)

matNMI = do.call(rbind, resNMI)
colnames(matNMI) =  c("kmeans","Louvain","Walktrap","BiCC + SPath","BiCC + SimJa","Central + SPath","Central + SimJa", "LeaderC + SPath","LeaderC + SimJa", "ILouvain")
write.table(matNMI, file = paste0(output,"Partitions/","ILouvainNMI",".csv"),row.names = TRUE, col.names=TRUE)



melted = melt(matARI)
ggplot(melted, aes(x = Var2, y = value, colour = Var2))+geom_boxplot() + ylab(" ARI")+ ylim(c(0,1))+
  ggtitle("\t\t Adjusted rand index of the evaluated methods on 100 benchmark dataset") +
  guides(color = guide_legend(ncol=1,title ="Algorithms :" ))  + theme(panel.background =  element_rect(fill = 'white'),axis.text.x = element_text(angle=75),
                                                                       axis.line = element_line(), panel.grid.major = element_line(colour = "grey90"))
ggsave(paste0(PathFigureSaving,"ALLARI",".eps"))


melted = melt(matNMI)
ggplot(melted, aes(x = Var2, y = value, colour = Var2))+geom_boxplot() + ylab(" NMI")+ ylim(c(0,1))+
  ggtitle("\t\t NMI of the evaluated methods on 100 benchmark dataset") + xlab("") +
  guides(color = guide_legend(ncol=1,title ="Algorithms :" )) + theme(panel.background =  element_rect(fill = 'white'),axis.text.x = element_text(angle=75),
                                                                      axis.line = element_line(), panel.grid.major = element_line(colour = "grey90"))
ggsave(paste0(PathFigureSaving,"ALLNMI",".eps"))


matARIC=matARI[,c(1:3,6,10)]
tmp = as.vector(colnames(matARIC))
tmp[4]="ANCA"
colnames(matARIC) = tmp
melted = melt(matARIC)
ggplot(melted, aes(x = Var2, y = value, colour = Var2))+geom_boxplot() + ylab(" ARI")+ ylim(c(0,1))+
  ggtitle("\t\t Adjusted rand index of the evaluated methods on 100 benchmark dataset") + xlab("") +
  guides(color = guide_legend(ncol=1,title ="Algorithms :" ))  + theme(panel.background =  element_rect(fill = 'white'),axis.text.x = element_text(angle=75),
                                                                       axis.line = element_line(), panel.grid.major = element_line(colour = "grey90"))
ggsave(paste0(PathFigureSaving,"ILouvainARI",".eps"))

matNMIC=matNMI[,c(1:3,6,10)]
tmp = as.vector(colnames(matNMIC))
tmp[4]="ANCA"
colnames(matNMIC) = tmp
melted = melt(matNMIC)
ggplot(melted, aes(x = Var2, y = value, colour = Var2))+geom_boxplot() + ylab(" NMI")+ ylim(c(0,1))+
  ggtitle("\t\t NMI of the evaluated methods on 100 benchmark dataset") + xlab("") +
  guides(color = guide_legend(ncol=1,title ="Algorithms :" )) + theme(panel.background =  element_rect(fill = 'white'),axis.text.x = element_text(angle=75),
                                                                      axis.line = element_line(), panel.grid.major = element_line(colour = "grey90"))
ggsave(paste0(PathFigureSaving,"ILouvainNMI",".eps"))
