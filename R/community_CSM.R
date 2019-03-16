
#' CSM
#'
#' @description Collaborative similarity measure algorithm as describe in Nawaz, W., Khan, K. U., Lee, Y. K., & Lee, S. (2015). Intra graph clustering using collaborative similarity measure. Distributed and Parallel Databases, 33(4), 583-603.
#' @usage community.CSM(graph, alpha, k, itermax)
#' @param graph : an igraph graph
#' @param alpha :
#' @param k : Number of clusters
#' @param itermax : Number of maximal iterations (used when the algorithm don't converge)
#' @return A set of vertices clusters
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' graph = Polblogs
#' graph = delete_vertex_attr(graph,"source")
#' graph = delete_vertex_attr(graph,"label")
#' wt = community.CSM(graph, k = 3)
#' summary.partition.result(wt$membership)
#' @export




community.CSM <- function(graph, alpha = 0.5, k = 3,similarityFunction= similarity.matchingCoefficient,  method = "euclidean",itermax = 10){

  if(!is.igraph(graph))
    stop("not an igraph graph")
  if(is.null(vertex_attr(graph)$name))
    stop("graph must have a name vertex attribute ")

  VerticesNames = V(graph)$name
  n = vcount(graph)
  CSIM <- matrix(1,nrow = vcount(graph),ncol=vcount(graph))
  colnames(CSIM) <- VerticesNames
  rownames(CSIM) <-VerticesNames

  contextMatrix = tryCatch(similarityFunction(graph),error = function(e) as.matrix(similarityFunction(getAll.attribute(graph),method = method,diag=TRUE)))

  colnames(contextMatrix) <- VerticesNames
  rownames(contextMatrix) <-VerticesNames

  allDegree = degree(graph)
  cat(".")
  for(nodei in seq(n-1)){
    #cat(paste0(VerticesNames[nodei],"."))
    for(nodej in seq(nodei+1,n)){
      path <- as.vector(get.shortest.paths(graph,VerticesNames[nodei],VerticesNames[nodej])$vpath[[1]])
      if(length(path)!=0)
        CSIM[VerticesNames[nodei],VerticesNames[nodej]] <- alpha*prod(1/allDegree[path]) + (1-alpha)*prod(contextMatrix[VerticesNames[nodei],path])
        CSIM[VerticesNames[nodej],VerticesNames[nodei]] <-  CSIM[VerticesNames[nodei],VerticesNames[nodej]]
      }
    }

  dist = 1/CSIM
  cat(".")

  for(nodei in seq(n-1)){
    #cat(paste0(VerticesNames[nodei],"."))
    for(nodej in seq(nodei+1,n)){
      path <- as.vector(get.shortest.paths(graph,VerticesNames[nodei],VerticesNames[nodej])$vpath[[1]])
      if(length(path)==0){
        dist[VerticesNames[nodei],VerticesNames[nodej]] = Inf
        dist[VerticesNames[nodej],VerticesNames[nodei]] = Inf

      }
      else if(length(path)>2)
        dist[VerticesNames[nodei],VerticesNames[nodej]] = prod(CSIM[VerticesNames[nodei],path])
      dist[VerticesNames[nodej],VerticesNames[nodei]] = dist[VerticesNames[nodei],VerticesNames[nodej]]
    }
    dist[VerticesNames[nodei],VerticesNames[nodei]] <- 0
  }
  cat(".")

  iter = 0
  oldFobjective = 0
  Converge  = FALSE
  centers = sort(degree(graph), decreasing = TRUE)[1:k]

  # Clusters attribution
  clusters = vector()
  for(i in VerticesNames){
    clusters = c(clusters, names(sampleOne(which(dist[i,centers] == min(dist[i,centers])))))
  }
  names(clusters) <- VerticesNames

  while((iter > itermax) || Converge){
    #cat(paste0(iter,". "))

    #Convert membership to list of partition
    clusters <- memb2groups(clusters)

    Fobjective = (alpha * density.clusters(graph, clusters)) / ((1-alpha) * mean(entropy.attribute(graph, clusters)))
    #print(paste0(round(Fobjective,2)," - ",round(density.clusters(graph, clusters),2)," - ", round(mean(entropy.attribute(graph, clusters)),2)))

    centersUpdating <- vector()
    for( clust in clusters){
      if(length(clust)==1)
        centersUpdating <-  c(centersUpdating,clust)
      else
        centersUpdating <-  c(centersUpdating,names(which.min(sapply(clust, function(i){sum(dist[i , clust])}))))
    }

    centers = centersUpdating

    # Clusters attribution
    clusters = vector()
    for(i in VerticesNames){
      clusters = c(clusters, names(sampleOne(which(dist[i,centers] == min(dist[i,centers])))))
    }
    names(clusters) <- VerticesNames

    if(oldFobjective == Fobjective)
      Converge = TRUE

    oldFobjective = Fobjective
    iter = iter +1
    }
  cat("\n")
  return(create.communities(graph = graph, membership = as.numeric(clusters),algorithm="CSM"))
}



