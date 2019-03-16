


#' Internal density of a given partition
#'
#' @description Internal density of parititon
#' @usage internal.density.clusters(graph, partition)
#' @param graph : an igraph graph
#' @param partition : a partition membership
#' @return a numeric value
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' graph = DBLP10K
#' wt = ANCA(graph)
#' internal.density.clusters(graph,wt)
#' @export

internal.density.clusters <- function(graph, partition){
  if(!is.igraph(graph))
    stop("Should be applied on a igraph graph object")
  d = 0
  if(is.list(partition)){
    for(cluster in partition){
      if(length(cluster)<=1)
        next
      m =   ecount(induced_subgraph(graph,cluster))
      n = length(cluster)
      d = d + ((2 * m )/(n * (n - 1)))
    }
    return(d/length(partition))
  }
  else if(is.vector(partition)){
    density.clusters(graph,memb2groups(partition))
  }
  else
    stop("argument partition should be a list or a vector")

  #length(which(crossing(wt,graph)==FALSE))/ecount(graph  )
}
