
#' Entropy of parititon
#'
#' @description Entropy of parititon
#' @usage entropy.attribute(graph, partition)
#' @param graph : an igraph graph
#' @param partition : a partition membership
#' @return a numeric value
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' graph = DBLP10K
#' wt = ANCA(graph)
#' entropy.attribute(graph,wt)
#' @export


entropy.attribute <- function(graph, partition,categorical=TRUE){
  if(!is.igraph(graph))
    stop("Should be applied on a igraph graph object")
  if(is.list(partition)){
    entrop <- vector()
    if(categorical){
      for(attribute_name in setdiff(vertex_attr_names(graph),c("name","id"))){
        attribute_domaine = unique(get.vertex.attribute(graph,attribute_name))
        entropy = 0

        for(cluster in partition){
          subg <- induced_subgraph(graph,cluster)
          pct = vcount(subg)/vcount(graph)
          for(domaine in attribute_domaine){
            p = (length(which(vertex_attr(subg, attribute_name)==domaine))/vcount(subg))
            if(p>0){
              entropy = entropy + pct *  (p * log2(p))
            }
          }
        }
        entrop <- c(entrop, round(- entropy,3))
        #print(paste("Entropy for the attribute : ",attribute_name," = ", round(- entropy,2),sep=""))
        }
      #print(paste("The general entropy is  = ", round(mean(entrop),2),sep=""))
    }
    else{
      for(attribute_name in setdiff(vertex_attr_names(graph),c("name","id"))){
        attribute_domaine = unique(get.vertex.attribute(graph,attribute_name))
        entropy = 0

        for(cluster in partition){
          subg <- induced_subgraph(graph,cluster)
          pct = vcount(subg)/vcount(graph)
            p = mean(vertex_attr(subg, attribute_name))
            if(p>0)
              entropy = entropy + pct *  (p * log2(p))
        }
        entrop <- c(entrop, round(- entropy,3))
      }
    }
    names(entrop) <- setdiff(vertex_attr_names(graph),c("id","name"))
    return(entrop)
  }
  else if(is.vector(partition)){
    entropy.attribute(graph, memb2groups(partition))
  }
  else
    stop("argument should be a list or vector ")
}
