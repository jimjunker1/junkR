#' @description
#' A faster way to determine if all objects in a list are identical
#' @param ... a list of object of arbitrary length
#' @returns logical. TRUE if all objects are identical
#' @export
AllIdentical <- function(...){
  lst <- list(...)
  # identical ought to be transitive, so if A is identical to C and to D, then C should be identical to D

  # all(sapply(lst[-1], identical, lst[[1]]))

  # we might not need to compare all elements
  for(i in seq_along(lst)[-1]){

    if(!identical(lst[[i]], lst[[1]])){
      # we can stop after the first inequality
      return(FALSE)
    }
  }
  return(TRUE)

  # 3 times faster than original

  # library(microbenchmark)
  # microbenchmark(
  #   orig = AllIdentical(A, B, C, D, E),
  #   A = AllIdenticalA(A, B, C, D, E),
  #   times  = 2000L
  # )


}
