#' This function exchanges full author list formated with First and Middle initials
#' and full family name. This allows the creation of bib entries with full author
#' lists
#'
#' @param x bib entry created from RefManageR::ReadGS() (i.e., list) with incomplete
#' author list
#' @param y personList created from convert_complete_author(). Same length as x
#' @examples
#' author_exchange()
#' #mapply(author_exchange, x = jrj.bib, y = full_authors_list)
#' @importFrom RefManageR RelistBibEntry
#' @export
#'
author_exchange <- function(x,y){
  requireNamespace('RefManageR', quietly = TRUE)
  x_flat <- unlist(x)
  x_flat$author <- y
  return(RelistBibEntry(x_flat))
}
