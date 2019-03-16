#convert a partition to

groups2communities <- function(groups){
  if(!is.list(groups))
    stop("Should be a list of list")
  memb <- vector(length = length(unique(do.call(c,groups))))
  names(memb) <- unique(do.call(c,groups))

  for(i in seq(length(groups)))
      memb[c(groups[[i]])] <- i
      return(memb)
}


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
