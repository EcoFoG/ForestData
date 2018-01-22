library(dplyr)
forestData.guyafor_BasalArea <- function(data, forest, plot = F, subplot = F, taxon = F, year, surface, Dmin = F, Dmax = F) {
  data_temp <- data %>%
    filter(NomForet == forest) %>%
    filter(n_parcelle == plot) %>%
    filter(n_carre == subplot) %>%
    filter(idTaxon == taxon) %>%
    filter(campagne == year)

  if (Dmin != F) {
    data_temp <- data_temp %>%
      filter(circonf >= Dmin)
  } else if (Dmax != F) {
    data_temp <- data_temp %>%
      filter(circonf <= Dmax)
  }
  return(data_temp) # forestData.BasalArea(data_temp$circonf, surface))

}

forestData.BasalArea <- function(Measures, Surface) {
  BasalArea <- Measure^2/4/pi
  BasalArea <- sum(BasalArea)/Surface
  return(BasalArea)
}


forestData.guyafor_BasalArea(data = DataGuyafor,forest = "Paracou", plot=1, subplot =  1, year =  2016, surface = 500)
