
#' memb2groups
#'
#' @description Transform  the membership vector of a set of object to a list of numeric vector each one represent a community
#' @usage memb2groups(vect)
#' @param vect : Numeric vector, one value for each vertex, the membership vector of the community structure.
#' @return A list of numeric vectors.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @examples
#'
#'
#' @export


memb2groups <- function(vect){

  L <- list()
  if(is.null(names(vect))){
    for(i in unique(vect))
      L <- c(L, list(which(vect==i)))
  }
  else{
    for(i in unique(vect))
      L <- c(L,list( names(which(vect==i))))
  }
  return(L)

}
