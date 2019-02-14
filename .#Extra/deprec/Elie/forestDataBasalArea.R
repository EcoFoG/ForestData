#' Guyafor Basal Area
#'
#' @param data
#' @param forest
#' @param plot
#' @param subplot
#' @param taxon
#' @param year
#' @param surface
#' @param Dmin
#' @param Dmax
#'
#' @return
#'
#' @examples
#' forestData.guyafor_BasalArea(data = DataGuyafor, forest = "Paracou", plot=1, subplot =  1, year =  2016, surface = 500, Dmin = 20)
guyafor_BasalArea <- function(data, forest, plot = F, subplot = F, taxon = F, year, surface, Dmin = F, Dmax = F) { # Basal Area function for the Guyafor database (in dataframe)
  data_temp <- data %>%
    filter(NomForet == forest) %>%
    filter(campagne == year)

  if (plot != F) {
  data_temp <- data_temp %>%
    filter(n_parcelle == plot)
  }
  if (subplot != F) {
  data_temp <- data_temp %>%
    filter(n_carre == subplot)
  }
  if (taxon != F) {
  data_temp <- data_temp %>%
    filter(idTaxon == taxon)
  }
  if (Dmin != F) {
    data_temp <- data_temp %>%
      filter(circonf >= Dmin)
  } else if (Dmax != F) {
    data_temp <- data_temp %>%
      filter(circonf <= Dmax)
  }
  return(forestData.BasalArea(data_temp$circonf, surface))

}

#' Basal Area
#'
#' basalArea function calculate the basal area from 2 vectors of data
#'
#' @param Measures Measures vector contain a list of circumferences
#' @param Surface Surface contain a surface
#'
#' @return Numeric Value
#'
#' @examples
#' basalArea(c(20,25,30,70,40,20,35),50)
#' 15.99507
basalArea <- function(Measures, Surface) { # Generic Basal Area Function, with vectors
  BasalArea <- Measures^2/4/pi
  BasalArea <- sum(BasalArea)
  BasalArea <- BasalArea/Surface
  return(BasalArea)
}
