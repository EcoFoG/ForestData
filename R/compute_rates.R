#' Title
#'
#' @inheritParams correct_all
#' @param corrected Logical, indicates whether the dataset has been corrected
#'   (for tree status errors) beforehand. If TRUE, triggers correct_alive,
#'   defaults to TRUE.
#'
#' @return a data.frame that contains recruitment and mortality rates, in the
#'   same format as the outputs of compute_mortality and compute_recruitment
#' @export
compute_rates <- function(data,
                          status_col="status_corr",
                          time_col=ifelse(is.null(getOption("time_col")),
                                          "CensusYear",
                                          getOption("time_col")),
                          id_col=ifelse(is.null(getOption("id_col")),
                                        "idTree",
                                        getOption("id_col")),
                          dead_confirmation_censuses=2,
                          byplot = TRUE,
                          plot_col = ifelse(is.null(getOption("plot_col")),
                                            "Plot",
                                            getOption("plot_col")),
                          corrected = TRUE){


  mortality <- compute_mortality(data = data,
                                 status_col=status_col,
                                 time_col=time_col,
                                 id_col=id_col,
                                 dead_confirmation_censuses=dead_confirmation_censuses,
                                 byplot=byplot,
                                 plot_col=plot_col,
                                 corrected=corrected)

  recruitment <- compute_recruitment(data = data,
                                     status_col=status_col,
                                     time_col=time_col,
                                     id_col=id_col,
                                     dead_confirmation_censuses=dead_confirmation_censuses,
                                     byplot=byplot,
                                     plot_col=plot_col,
                                     corrected=corrected)

  merged <- .merge_rates(mortality, recruitment, by = c(time_col, plot_col))
  return(merged)
}
