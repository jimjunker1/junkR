#' @description
#' This function automates a trackdown upload to google drive based on the script of \item{upload_trackdown.R}
#' @param ul_filePattern the file pattern for the script used to automate the upload
#' @param force logical. should the function continue if multiple scripts are identified
#'
#'
#'
trackdown_upload <- function(ul_filePattern = "upload_trackdown.R",force = FALSE,...){
  uploadScript = list.files(pattern = ul_filePattern, full.names= TRUE, recursive = TRUE)
  if(all(length(uploadScript) > 1 & force == FALSE)) stop("Error: More than one upload script was identified. To proceed, rerun with force = TRUE")

  if(length(uploadScript) > 1 & force == TRUE){
    sapply(uploadScript, source)
  } else{
  source(uploadScript)
  }
}
