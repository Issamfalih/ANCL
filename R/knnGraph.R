

#'  k-nearest neighbor graph
#'
#' @description  k-nearest neighbor graph defined over the similarity or distance matrix
#' @usage knn_graph(dx,k,names,similarity)
#' @param similarity : if TRUE then dx is a similarity matrix else it is a distance matrix
#' @param names : the names of vertex in the new graph
#' @param k : integer the number of neighbors.
#' @param dx : matrix that express the similarity or distance between elemenst i and j
#' @return returns the Knn_graph defined over the similarity/Distance matrix dx
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#'
#' @export

knnGraph <- function(dx,k=3,names=NULL,similarity=TRUE){
  if(is.null(rownames(dx))||is.null(colnames(dx)))
    stop("The matrix dx should have row names and column names")
  if(ncol(dx)!=nrow(dx))
    stop("non square matrix")
  if(!isSymmetric(dx))
     stop("Should be symmetric matrix")
  if(!is.null(names)&&(ncol(dx)!=length(names)))
    stop("names should have the same length as matrix")
  n <- nrow(dx)
  if(is.null(names))
    names <- c(as.character(1:n))
  A <- matrix(0, nrow = n, ncol = n)#dimnames = list(names, names))
  rownames(A) <- names
  colnames(A) <- names
  for (i in 1:n){
    dis <- names(sort(dx[i, ][-i],decreasing = similarity))[1:k]
    A[i, dis ] <- A[i, dis ]+ 1
    #A[i, dis ] <-  1
  }
  diag(A) <- 0
  out <- simplify(graph.adjacency(A, mode = "undirected",weighted = TRUE),remove.multiple = TRUE,remove.loops = TRUE)

  return(set.vertex.attribute(out, "name", index=V(out), value=names))
}


