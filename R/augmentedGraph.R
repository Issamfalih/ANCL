



#' Augmented graph
#'
#' @description create an augmented graph based on the vertices attribute
#' @usage augmentedGraph(graph)
#' @param graph : an igraph graph
#' @return A igraph graph with attribute vertices
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' graph = DBLP10K
#' Agraph = augmentedGraph(graph)
#' summary.graph(Agraph)
#' @export


augmentedGraph <- function(graph){
  if(!is.igraph(graph))
    stop("not an igraph graph")
  if(is.null(vertex_attr(graph)$name))
    stop("graph must have a name vertex attribute ")
  copyGraph <- graph
  copyGraph <- set.vertex.attribute(copyGraph,name = "TopoOrAttr",index = V(copyGraph),value = "Topo")
  for(attrName in vertex_attr_names(graph)){
    if(attrName=="name" || attrName=="id" ) next
    value = get.vertex.attribute(graph,name = attrName)
    copyGraph <- add_vertices(copyGraph,length(unique(value)), name = sapply(unique(value),function(x){ paste0(attrName, "_", x)}), TopoOrAttr = "Attr")%>%
      add_edges(as.vector(t(data.frame(V(graph)$name,sapply(value,function(x){ paste0(attrName, "_", x)})))))
  }
  return(copyGraph)
}
