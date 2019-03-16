
#' ANCA Algorithm
#'
#' @description ANCA Algorithm as describ in ...
#' @usage (graph, nbCluster, nbEigenVectorSeeds, nbEigenVectorView,binarization, alpha, seedsFunction, structuralSimilarityFunction, structuralIsSimilarity, contextView, structuralView,cutoff, contextSimilarityFunction, method,iter.max,normalization, verbose)
#' @param nbCluster : integer number of cluster
#' @param graph : an igraph graph
#' @return A set of cluster
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @examples
#'
#'
#' @export


# seedsfunction = DetectSeeds_Bicomponents , DetectSeeds_Centrality
#topoSim = distances
ANCA <- function(graph, nbCluster=100, nbEigenVectorSeeds=2, nbEigenVectorView=2,binarization=FALSE, alpha = 0.5, seedsFunction = DetectSeeds_Centrality,
                 structuralSimilarityFunction = distances, structuralIsSimilarity=FALSE, contextView=TRUE, structuralView=TRUE,cutoff = 18,
                 contextSimilarityFunction=similarity.matchingCoefficient, method = "euclidean",iter.max=100,normalization=TRUE, verbose=FALSE){

  if(!is.igraph(graph))
    stop("Graph should be an igraph graph")
  if(!is.connected(graph)&& verbose)
    cat("Graph is not connected",sep = "\n")
  if(contextView && length(vertex_attr_names(graph))<=1)
    stop("Graph must have a vertex attribute")
  if(!("name" %in% vertex_attr_names(graph)))
    stop("Graph must have a name vertex attribute")
  n = vcount(graph)
  verticesName = V(graph)$name

  if(structuralView){
    if( verbose)
      cat("Step 1 : Seeds Selection",sep = "\n")
    Seeds  <- seedsFunction(graph)
    if( verbose)
      cat(paste(length(Seeds)," seeds founded ",sep=""),sep = "\n")
    if(length(Seeds)==0)
      stop("No seeds-nodes found ")
    if( verbose)
      cat("Step 2 : Characterize each node with the set of seeds using measure",sep = "\n")

    topoMatrix =  tryCatch(structuralSimilarityFunction(graph,to=Seeds),error = function(e) tryCatch(
                  structuralSimilarityFunction(graph)[,which(verticesName%in%Seeds)],
                        error = function(e) structuralSimilarityFunction(graph,cutoff=cutoff)))

    #topoMatrix =  structuralSimilarityFunction(graph,to=Seeds)
    if(!is.connected(graph)){
      topoMatrix[topoMatrix==Inf] = max(topoMatrix[is.finite(topoMatrix)]) + 1
    }

    if(structuralIsSimilarity)
        topoMatrix = 1 - topoMatrix
    if( verbose)
      cat("Step 3 : Matrix factorization on topological distance",sep = "\n")
    UTopo <- rsvd(topoMatrix, k=nbEigenVectorSeeds )$u
  }
  if(contextView){
    attrMatrix =  tryCatch(contextSimilarityFunction(graph),
                           error = function(e) contextSimilarityFunction(getAll.attribute(graph,binarization),method = method,diag=TRUE))
    #LAttrMatrix = diag(rowSums(attrMatrix)^(-0.5)) %*% attrMatrix %*% diag(rowSums(attrMatrix)^(-0.5))
    if( verbose)
      cat("Step 4 : Matrix factorization on attribute distance",sep = "\n")
    UAttr <- rsvd(attrMatrix,k = nbEigenVectorView)$u
    #    UAttr <- trlan.eigen(attrMatrix)$u[,1:nbEigenVectorView]
    U <- cbind(UTopo,UAttr )
  }
  else
    U = UTopo

  #Normalisation
  if( verbose)
    cat("Step 5 : Normalization",sep = "\n")
  if(normalization)
    U <- t(t(U)/sqrt(colSums(U^2)))

  #Cluster each row via K-means
  if( verbose)
    cat("Step 6 : Cluster each row",sep = "\n")

  if(length(nbCluster)==1){
    vect <- kmeans(U,nbCluster,iter.max = iter.max)$cluster
    #vect <- pam(U,nbCluster)$clustering
    names(vect) <- as.vector(get.vertex.attribute(graph,name = "name"))
    # cat(paste0("The execution time is :",dump(P)$time - P$time),sep = "\n")
    # cat(paste0("The memory used is :",  P$memory),sep = "\n")

    return(vect)
  }
  else{
    partition = list()
    for(k in nbCluster){
      vect <- kmeans(U,k,iter.max = iter.max)$cluster
      #vect <- pam(U,nbCluster)$clustering
      names(vect) <- as.vector(get.vertex.attribute(graph,name = "name"))
      partition = c(partition, list(vect))
      #cat(paste0("The execution time is :",dump(P)$time - P$time),sep = "\n")
      #cat(paste0("The memory used is :",  P$memory),sep = "\n")
    }
    return(partition)
  }

}


DetectSeeds_Bicomponents <- function(graph,centralityfunction=degree){
  if(!is.igraph(graph))
    stop("not an igraph graph")

  Seeds <- vector()
  bicomponent <- biconnected_components(graph)
  articulation_points <- bicomponent$articulation_points
  for(component in bicomponent$components){
    if(length(component)>1){
      Seeds <- c(Seeds, names(sort(centralityfunction(graph,component),decreasing = TRUE)[1:round(0.15*length(component))]))
      Seeds <- c(Seeds,names(sort(centralityfunction(graph,component),decreasing = FALSE)[1:round(0.05*length(component))]))
    }
  }
  return(unique(c(Seeds,names(articulation_points))))
}

DetectSeeds_Centrality <- function(graph,topPct=0.15, downPct=0.05){
  if(!is.igraph(graph))
    stop("not an igraph graph")
  Seeds <- vector()
  for(centrality in c("centralization.evcent(graph)$vector","page_rank(graph)$vector", "centralization.degree(graph)$res")){#,"centralization.closeness(graph)$res")){
    nodesCentralityList <-  eval(parse(text=centrality))
    names(nodesCentralityList) <- V(graph)$name
    Seeds <- c(Seeds, names(sort(nodesCentralityList,decreasing = TRUE)[1:round(topPct*vcount(graph))]))
    Seeds <- c(Seeds,names(sort(nodesCentralityList,decreasing = FALSE)[1:round(downPct*vcount(graph))]))
  }
  return(unique(Seeds))
}

DetectSeeds_Centrality_ <- function(graph,topPct=0.15, downPct=0.05){
  if(!is.igraph(graph))
    stop("not an igraph graph")
  Seeds <- vector()
  for(centrality in c("centralization.evcent(graph)$vector","page_rank(graph)$vector", "centralization.degree(graph)$res")){#,"centralization.closeness(graph)$res")){
    nodesCentralityList <-  eval(parse(text=centrality))
    names(nodesCentralityList) <- V(graph)$name
    Seeds <- c(Seeds, names(sort(nodesCentralityList,decreasing = TRUE)[1:round(topPct*vcount(graph))]))
    Seeds <- c(Seeds,names(sort(nodesCentralityList,decreasing = FALSE)[1:round(downPct*vcount(graph))]))
  }
  TSeeds = table(Seeds)
  return(TSeeds[TSeeds>=2])
}


DetectSeeds_Centrality_leaders <- function(graph,sigma=0.8){
  if(!is.igraph(graph))
    stop("not an igraph graph")
  Seeds <- vector()
  for(centrality in c("centralization.evcent(graph)$vector","page_rank(graph)$vector", "centralization.closeness(graph)$res",
                      "centralization.degree(graph)$res")){
    nodesCentralityList <-  eval(parse(text=centrality))
    #sigma <- ((mean(degree(graph)) + max(degree(graph))) /2)/vcount(graph)
    for(v in V(graph)$name){
      v_index = which(get.vertex.attribute(graph,"name")==v)
      if(isASeed(graph,v_index,nodesCentralityList,sigma=sigma)){
        Seeds <- c(Seeds,v)
      }
    }
  }
  return(unique(Seeds))
}

isASeed <- function(graph,node_index, nodesCentralityList,sigma){
  taux =  0
  neigh <- neighbors(graph,node_index)
  if(length(neigh)<3){

    return(FALSE)
  }
  for(neighbor in neigh ){
    if(nodesCentralityList[node_index] >= nodesCentralityList[neighbor]){
      taux <-  taux + 1.0
    }
  }
  #print(taux/(1+length(neighbors)))
  if((taux/(1+length(neigh)))>sigma)
    return(TRUE)
  return(FALSE)
}



