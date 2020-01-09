# small standalone functions

'%ni%' <- Negate('%in%')

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

C_to_overkt <<- function(a){1/(8.61733*10^-5*(a+273.15))}#overkt function
overkt_to_C <<- function(a){1/(a*(8.61733*10^-5)) - 273.15}
C_to_overkt_stand15 <<- function(a){(1/(8.61733e-5*(15+273.15)) - (1/(8.61733e-5*(a+273.15))))}

na.rm_mean <- function(...,na.rm=FALSE){mean(c(...),na.rm=na.rm)}

rmap <- function (.x, .f, ...) {
  if(is.null(dim(.x))) stop("dim(X) must have a positive length")
  .x <- t(.x) %>% as.data.frame(.,stringsAsFactors=F)
  purrr::map(.x=.x,.f=.f,...)
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

hello <- function(){print('olleh')}

makeNamedList <- function(...) {
  structure(list(...), names = as.list(substitute(list(...)))[-1L])
}

