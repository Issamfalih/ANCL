



#' sample
#'
#' @description choose randomly one object in a list of object
#' @usage sampleOne(x)
#' @param x : a vector
#' @return one object picked randomly
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @examples
#' sampleOne(c(0,8,1))'
#' @export



sampleOne <- function(x) {
  if (length(x) <= 1) {
    return(x)
  } else {
    return(sample(x,1))
  }
}

