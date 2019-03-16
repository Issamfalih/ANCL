#' multiplexbasedapproach
#'
#' @description
#' @usage
#' @param graph : an igraph graph
#' @return A igraph graph with attribute vertices
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#'
#'
#' @export






multiplexbasedapproach <-  function(graph,  neighborsNumber=NULL, attributeSimilarityFunction=similarity.matchingCoefficient,
                                multiplexCommunityAlgorithm = community.layer_aggregation, similarityGraph = knnGraph){

  if(!is.igraph(graph))
    stop("not an igraph graph")
  if(is.null(vertex_attr(graph)$name))
    stop("graph must have a name vertex attribute ")
  M <- new("Multiplex")
  M <- add_layer(M,graph)

  attr = getAll.attribute(graph)
  VerticesNames = V(graph)$name

  attrMatrix =  as.matrix(tryCatch(attributeSimilarityFunction(graph),
                         error = function(e) attributeSimilarityFunction(getAll.attribute(graph,binarization),method = method,diag=TRUE)))
  rownames(attrMatrix) <- VerticesNames
  colnames(attrMatrix) <- VerticesNames
  if(is.null(neighborsNumber))
      neighborsNumber = round(mean(degree(graph)))

  M <- add_layer(M,similarityGraph(as.matrix(attrMatrix),k = neighborsNumber,names = VerticesNames,similarity = TRUE))
  return(multiplexCommunityAlgorithm(M))
}
