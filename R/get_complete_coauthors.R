#' This function scrapes the full coauthor list for a publication from
#' Google Scholar. It is a modified version from the scholar package that
#' includes a sleep option to allow a time between http requests.
#'
#' @return A character string with a full author list of full author names
#' @param id A Google scholar id from a public id page
#' @param pubid A publication id can be returned from get_publications()
#' @param sleep A number in seconds to wait in between requests passes to Sys.sleep()
#' @param ... further argments to pass such as agent identification, encoding, etc.
#' @importFrom httr set_config user_agent
#' @importFrom xml2 read_html
#' @importFrom rvest html_node html_text
#' @export

get_complete_coauthors <- function (id, pubid, sleep = 3,...){
  httr::set_config(httr::user_agent(agent = "james.junker@montana.edu; +https://jimjunker.com"))
  auths = ""
  url_template = "http://scholar.google.com/citations?view_op=view_citation&citation_for_view=%s:%s"
  url = sprintf(url_template, id, pubid)
  Sys.sleep(sleep)
  url1 <- xml2::read_html(url)
  auths = as.character(rvest::html_node(url1, ".gsc_vcd_value") %>%
                         rvest::html_text())
  return(auths)
}


