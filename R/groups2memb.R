

#' groups2memb
#'
#' @description Transform  list of numeric vector each one represent a community  to  membership vector.
#' @usage groups2memb(groups)
#' @param A list of numeric vectors.
#' @return  vect : Numeric vector, one value for each vertex, the membership vector of the community structure.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @examples
#'
#'
#' @export


groups2memb<- function(groups){
  if(!is.list(groups))
    stop("Should be a list of list")
  memb <- vector(length = length(unique(do.call(c,groups))))
  names(memb) <- unique(do.call(c,groups))

  for(i in seq(length(groups)))
    memb[c(groups[[i]])] <- i
  return(memb)
}


