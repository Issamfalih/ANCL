#' Graph weighted
#'
#' @description Graph weighted by the node attriute similarity
#' @usage GraphWeighted_NodeAttribute(graph, similarityFunction)
#' @param graph : an igraph graph with node attribute information
#' @param similarityFunction : a node similarity function
#' @return An igraph weighted graph
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' graph = DBLP10K
#' GraphWeighted_NodeAttribute(graph)
#' @export

GraphWeighted_NodeAttribute  <- function(graph, similarityFunction = matchingCoefficient){
  if(!is.igraph(graph))
    stop("not an igraph graph")
  if(is.null(vertex_attr(graph)$name))
    stop("graph must have a name vertex attribute ")

  df = data.frame(row.names = V(graph)$name)
  for(attrName in setdiff(vertex_attr_names(graph),c("name","id")))
    df <- cbind(df,attrName = get.vertex.attribute(graph,name = attrName,index = V(graph)$name))


  graphCopy = set.edge.attribute(graph,name ="weight",index = E(graph),
                                 value =   sapply(split(t(get.edgelist(graph)), rep(1:ecount(graph), each = 2)),
                                                  FUN = function(x){1 - similarityFunction(df[as.character(x[1]),],df[as.character(x[2]),]) }))

  return(graphCopy)
}

