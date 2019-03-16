

#' Conductance
#'
#' @description The average conductance of a prtition
#' @usage conductance(graph, partition)
#' @param graph : an igraph graph
#' @param partition : a partition membership
#' @return a numeric value
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#'  graph = DBLP10K
#'  wt = ANCA(graph)
#'  conductance(graph,wt)
#' @export


conductance <- function(graph, partition){

  if(!is.igraph(graph))
    stop("Should be applied on a igraph graph object")
  cond = vector()
  if(is.list(partition)){
    adj = get.adjacency(graph,type = "both",names = TRUE)
    verticesNames = colnames(adj)
    for(com in partition)
      cond = c(cond, sum(adj[com,verticesNames[!(verticesNames %in% com)]])/min( sum(adj[com,]),sum(adj[verticesNames[!(verticesNames %in% com)],])))
    return(1 - sum(cond,na.rm = TRUE)/length(partition))
  }
  else if(is.vector(partition)){
    conductance(graph,memb2groups(partition))
  }
  else
    stop("argument partition should be a list or a vector")
}
