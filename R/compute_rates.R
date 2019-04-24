compute_rates <- function(data,
                          status_col="status_corr",
                          time_col="CensusYear",
                          id_col="idTree",
                          dead_confirmation_censuses=2,
                          byplot = TRUE,
                          plot_col = "Plot",
                          corrected = TRUE){


  mortality <- compute_mortality(data,
                                 status_col,
                                 time_col,
                                 id_col,
                                 dead_confirmation_censuses,
                                 byplot,
                                 plot_col,
                                 corrected)
  recruitment <- compute_recruitment(data,
                                 status_col,
                                 time_col,
                                 id_col,
                                 dead_confirmation_censuses,
                                 byplot,
                                 plot_col,
                                 corrected)
  merged <- .merge_rates(mortality, recruitment, by = c(time_col, plot_col))

}
