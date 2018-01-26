library(dplyr)
forestData.guyafor_BasalArea <- function(data, forest, plot = F, subplot = F, taxon = F, year, surface, Dmin = F, Dmax = F) { # Basal Area function for the Guyafor database (in dataframe)
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

forestData.BasalArea <- function(Measures, Surface) { # Generic Basal Area Function, with vectors
  BasalArea <- Measures^2/4/pi
  BasalArea <- sum(BasalArea)
  BasalArea <- BasalArea/Surface
  return(BasalArea)
}


forestData.guyafor_BasalArea(data = DataGuyafor, forest = "Paracou", plot=1, subplot =  1, year =  2016, surface = 500, Dmin = 20)
