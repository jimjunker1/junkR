# small standalone functions
#' @description
#' This function is the opposite of \code{\%in\%} in that it finds items not in a vector
#' @title \%ni\%
#' @param x vector or NULL: the values to exclude. Long vectors are supported.
#' @param table vector or NULL: the values to be excluded against. Long vectors are not supported.
#' #' @returns A logical vector, indicating if a match was not located for each element of x: thus the values are TRUE or FALSE and never NA.
`%ni%` <- function(x, table) {!(x %in% table)}
# '%ni%' <- Negate('%in%')

#' temperature conversion functions
#' @description
#' These functions convert among temperatures
#' @param a temperature in degress celcius
C_to_overkt <- function(a = NULL){1/(8.61733*10^-5*(a+273.15))}#overkt function

#' absolute temperature conversion funciton
#' @description
#' Function to convert inverse absolute temperature to celcius
#'
#' @param a temperature in inverse boltzmann temperature
overkt_to_C <- function(a = NULL){1/(a*(8.61733*10^-5)) - 273.15}

#' C_to_overkt_stand
#' @description
#' This functions converts to celcius to inverse Boltzmann temperature (1/kT) from a set standard, e.g., 15 C
#' @param a This is the temperature in celcius
#' @param mu This is the standardization temperature in celcius
C_to_overkt_stand <- function(a = NULL, mu = 15){(1/(8.61733e-5*(mu+273.15)) - (1/(8.61733e-5*(a+273.15))))}

#' calculate the mode
#' @description
#' This function calculates the model of a vector
#' @param x vector to calculate the mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' get_legend: get the legend from ggplot
#' @description
#' This function extracts the legend from a ggplot for complex plotting layouts or exporting for presentations
#' @param myggplot This is the ggplot object to extract the legend from
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


theme_mod <- theme_bw() %+replace% theme(panel.grid = element_blank())
theme_black <- function() {theme_bw() %+replace% theme(panel.background = element_rect(fill = 'transparent', colour = NA),panel.grid = element_blank(), axis.ticks = element_line(color = 'white'),
                                                        axis.title = element_text(color = 'white'), axis.text = element_text(color = 'white'), plot.background =element_rect(fill = 'transparent', colour = NA),
                                                        panel.border = element_rect(fill = NA, colour = 'white'))}
