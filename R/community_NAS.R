
#' Node Attribute Similarity  (NAS)
#'
#' @description NAS as describe in Steinhaeuser, K., & Chawla, N. V. (2008). Community detection in a large real-world social network.
#'  Social computing, behavioral modeling, and prediction, 168-175.
#' @usage community.NAS(graph,threshold)
#' @param graph : an igraph graph with node attribute information
#' @param threshold in the range (0,1), we place any pair of nodes i and j whose edge weight exceeds the threshold in the same community.
#' @return returns an igraph  communities object. A vertice partition of the input graph.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' graph = DBLP10K
#' wt = community.NAS(graph)
#' @export


community.NAS <- function(graph,threshold = 0.3){

  if(!is.igraph(graph))
    stop("Graph should be an igraph graph")
  if(!is.connected(graph))
    cat("Graph is not connected",sep = "\n")
  if(!("name" %in% vertex_attr_names(graph)))
    stop("Graph must have a name vertex attribute")

  CpyGraph = GraphWeighted_NodeAttribute(graph)

  return(create.communities(graph = graph, membership = clusters(delete_edges(CpyGraph,E(CpyGraph)[weight<threshold]))$membership,algorithm="NAS"))
}
