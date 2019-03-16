
#' Binarize
#'
#' @description  binairization of categorical data
#' @usage binarize(graph)
#' @param graph : an igraph graph
#' @return A dataframe
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @examples
#'
#'
#' @export


binarize <- function(graph){

  if(!is.igraph(graph))
    stop("not an igraph graph")
  if(is.null(vertex_attr(graph)$name))
    stop("graph must have a name vertex attribute ")

  attrDf = getAll.attribute(graph)

  attr = data.frame(row.names = V(graph)$name)
  for(attrName in colnames(attrDf)){
    for (x in unique(attrDf[,attrName]))
      eval(parse(text=paste0( " attr[paste0(attrName,\"_\", x)] <- as.numeric(attrDf[,attrName] == x) ")))
  }
  return(attr)
}
