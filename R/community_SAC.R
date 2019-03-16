#'SAC Algorithm
#'
#' @description SAC as descib in ang, The Anh, & Viennet, Emmanuel. (2012). Community detection based on structural and attribute similarities.
#' Pages 7–14 of: International conference on digital society (icds). ISBN: 978-1-61208-176-2.
#' @usage community.SAC(graph, alpha,contextSimilarityFunction)
#' @param alpha : in (0,1) is a weighting factor
#' @param graph : an igraph graph
#' @return returns an igraph  communities object. A vertice partition of the input graph.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' graph = DBLP10K
#' wt = community.SAC(graph,0.5)
#' @export


community.SAC  <- function(graph, alpha=0.5, contextSimilarityFunction=similarity.matchingCoefficient, binarization=FALSE,method = "euclidean"){

  if(!is.igraph(graph))
    stop("not an igraph graph")
  if(is.null(vertex_attr(graph)$name))
    stop("graph must have a name vertex attribute ")


  topoMatrix =  get.adjacency(graph)

  attrMatrix =  tryCatch(contextSimilarityFunction(graph),
                         error = function(e) contextSimilarityFunction(getAll.attribute(graph,binarization),method = method,diag=TRUE))


  distanceMatr <- alpha * as.matrix(topoMatrix) +  (1 - alpha) * as.matrix(attrMatrix)

  SimGraph <- knnGraph(as.matrix(distanceMatr),k = round(mean(degree(graph))),names = V(graph)$name,similarity = TRUE)


  return(create.communities(graph = graph, membership = cluster_louvain(SimGraph)$membership,algorithm="SAC"))

}
