# small standalone functions
#' @description
#' This function is the opposite of \code{\%in\%} in that it finds items not in a vector
#' @title \%ni\%
#' @param x vector or NULL: the values to exclude. Long vectors are supported.
#' @param table vector or NULL: the values to be excluded against. Long vectors are not supported.
#' #' @returns A logical vector, indicating if a match was not located for each element of x: thus the values are TRUE or FALSE and never NA.
`%ni%` <- function(x, table) {!(x %in% table)}
# '%ni%' <- Negate('%in%')

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

C_to_overkt <- function(a = NULL){1/(8.61733*10^-5*(a+273.15))}#overkt function
overkt_to_C <- function(a = NULL){1/(a*(8.61733*10^-5)) - 273.15}
C_to_overkt_stand15 <- function(a = NULL, mu = 15){(1/(8.61733e-5*(mu+273.15)) - (1/(8.61733e-5*(a+273.15))))}

na.rm_mean <- function(...,na.rm=FALSE){mean(c(...),na.rm=na.rm)}

#'
#' @importFrom purrr map
rmap <- function (.x, .f, ...) {
  if(is.null(dim(.x))) stop("dim(X) must have a positive length")
  .x <- t(.x) %>% as.data.frame(.,stringsAsFactors=F)
  map(.x=x,.f=.f,...)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

makeNamedList <- function(...) {
  structure(list(...), names = as.list(substitute(list(...)))[-1L])
}

theme_mod <- theme_bw() %+replace% theme(panel.grid = element_blank())
theme_black <- function() {theme_bw() %+replace% theme(panel.background = element_rect(fill = 'transparent', colour = NA),panel.grid = element_blank(), axis.ticks = element_line(color = 'white'),
                                                        axis.title = element_text(color = 'white'), axis.text = element_text(color = 'white'), plot.background =element_rect(fill = 'transparent', colour = NA),
                                                        panel.border = element_rect(fill = NA, colour = 'white'))}
