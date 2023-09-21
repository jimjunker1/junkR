#' @title trackdown_download
#' @description
#' This function automates a trackdown download from google drive based on the script of \code{download_trackdown.R}
#' @param dl_filePattern the file pattern for the script used to automate the download
#' @param force logical. should the script force the download if multiple scripts are located
#' @param ... additional arguments passed to function
#' @returns nothing
#' @export
#'
trackdown_download <- function(dl_filePattern = "download_trackdown.R", force = FALSE,...){
  downloadScript = list.files(pattern = dl_filePattern, full.names= TRUE, recursive = TRUE)
  if(all(length(downloadScript) > 1 & force == FALSE)) stop("Error: More than one download script was identified. To proceed, rerun with force = TRUE")

  if(length(downloadScript) > 1 & force == TRUE){
    sapply(downloadScript, source)
  } else{
    source(downloadScript)
  }
}

#' @title trackdown_upload
#' @description
#' This function automates a trackdown upload to google drive based on the script of \code{upload_trackdown.R}
#' @param ul_filePattern the file pattern for the script used to automate the upload
#' @param force logical. should the function continue if multiple scripts are identified
#' @param ... additional argument passed to function
#' @returns nothing
#' @export
trackdown_upload <- function(ul_filePattern = "upload_trackdown.R",force = FALSE,...){
  uploadScript = list.files(pattern = ul_filePattern, full.names= TRUE, recursive = TRUE)
  if(all(length(uploadScript) > 1 & force == FALSE)) stop("Error: More than one upload script was identified. To proceed, rerun with force = TRUE")

  if(length(uploadScript) > 1 & force == TRUE){
    sapply(uploadScript, source)
  } else{
    source(uploadScript)
  }
}
