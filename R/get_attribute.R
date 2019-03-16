#' get all attribute
#'
#' @description return all vertices attribute of a given graph
#' @usage getAll.attribute(graph,binarization, rowNames)
#' @param graph : an igraph graph
#' @param binarization : logical
#' @param rowNames : vector of nodes name
#' @return returns a dataframe containing for each vertex (in rows) all the attribute value ( in columns)
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' graph = DBLP10K
#' getAll.attribute(graph)
#' @export



getAll.attribute <- function(graph,binarization = FALSE,rowNames=NULL){

  if(!is.igraph(graph))
    stop("not an igraph graph")
  if(is.null(vertex_attr(graph)$name))
    stop("graph must have a name vertex attribute ")
  if(binarization)
    return(binarize(graph))
  if(is.null(rowNames))
    rowNames = V(graph)$name
  return(as.data.frame(sapply(setdiff(vertex_attr_names(graph),c("name" ,"id")),function(attrName){get.vertex.attribute(graph,name = attrName)})))
}
