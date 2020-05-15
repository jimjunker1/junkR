#' This function converts full author lists to person objects.
#'
#' It can be used to import full author person objects into bib objects
#' returned from RefManageR::ReadGS. This further compresses the full author
#' names into just the first character for given and middle names.
#' @param complete_coauthor_string A character string of the full author list
#' separated by ,s of the form e.g., James Robert Junker, John Jacob Jingle Heimer-Schimdt, etc.

convert_complete_author <- function(complete_coauthor_string){

  x <- complete_coauthor_string
  author_lists <- lapply(strsplit(x, ","), stringr::str_trim)
  author_lists <- lapply(author_lists, function(x) sapply(x, strsplit, " "))
  author_persons <- lapply(author_lists, function(x) lapply(x, function(name){
    if(length(name) == 2){
      person(given = substr(name[1], start = 1, stop = 1), family = name[2], middle = NULL)
    } else if(length(name) == 3){
      person(given = substr(name[1],start =1, stop = 1), family = name[3], middle = substr(name[2], start = 1, stop = 1))
    } else{
      person(given = name[1], family = length(name), middle = paste0(substr(name[2:(length(name)-1)], start = 1, stop = 1)))
    }}))

}
