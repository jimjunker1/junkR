#' @description
#' This function automates a trackdown download from google drive based on the script of \item{download_trackdown.R}
#' @param dl_filePattern description
#' @param force should the script force the download if multiple scripts are located?
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
