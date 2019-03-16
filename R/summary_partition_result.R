
#' summary result of a partition
#'
#' @description A summary result of a given partition
#' @usage summary.partition.result(graph,partition)
#' @param graph : an igraph graph with node attribute information
#' @param partition : vertices parition of the graph
#' @return returns the results of clusters validation indexes
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' graph = Polblogs
#' wt = ANCA(graph)
#' summary.partition.result(graph,wt)
#' @export


summary.partition.result <- function(graph,partition){


  if(!is.igraph(graph))
    stop("Graph should be an igraph graph")

  result = vector()
  if(is.list(partition))
    result= c(result,round(modularity(graph,groups2memb(partition)),4))
  else
    result= c(result,round(modularity(graph,partition),4))

  result= c(result,round(mean(entropy.attribute(graph,partition)),4))

  result= c(result,round(density.clusters(graph,partition),4))

  result= c(result,round(conductance(graph,partition),4))

  if(!is.list(partition)){
    result= c(result,length(memb2groups(partition)))
    result= c(result,round(mean(sapply(memb2groups(partition),length))))
  }
  else{
    result= c(result,length(partition))
    result= c(result,round(mean(sapply(partition,length))))
  }

  names(result) = c("Modularity","Entropy","Density","Conductance","size","avgsize")

  return(result)

}
