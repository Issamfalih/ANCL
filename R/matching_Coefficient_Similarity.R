




#' matching coefficient
#'
#' @description  matching coefficient similarity measure
#' @usage matchingCoefficient(vect1,vect2,normalization)
#' @param vect1 : vector of attribute values
#' @param vect2 : vector of attribute values with same length of vect1
#' @param normalization :
#' @return numeric value that represent the similarity value
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#'
#'
#' @export


similarity.matchingCoefficient <- function(graph, normalization=TRUE){
  attr = getAll.attribute(graph)
  if(normalization)
      return(do.call(rbind,lapply(V(graph)$name, function(i){rowSums(sapply(lapply(seq(ncol(attr)), function(j){ attr[,j]==attr[i,j] }),rbind))}))/ncol(attr))
  else
      return(do.call(rbind,lapply(V(graph)$name, function(i){rowSums(sapply(lapply(seq(ncol(attr)), function(j){ attr[,j]==attr[i,j] }),rbind))})))

}

similarity.euclidean <- function(graph, normalization=TRUE){
  attr = getAll.attribute(graph)
  if(normalization)
    return(do.call(rbind,lapply(V(graph)$name, function(i){rowSums(sapply(lapply(seq(ncol(attr)), function(j){ attr[,j]==attr[i,j] }),rbind))}))/ncol(attr))
  else
    return(do.call(rbind,lapply(V(graph)$name, function(i){rowSums(sapply(lapply(seq(ncol(attr)), function(j){ attr[,j]==attr[i,j] }),rbind))})))

}


matchingCoefficient <- function(vect1, vect2, normalization=TRUE){
  if(length(vect1)!=length(vect2))
    stop(" vect1 and vect2  should have the same length")
  count=length(which((vect1==vect2)==TRUE))
  if(normalization)
    return(count / length(vect1))
  return(count)
}




