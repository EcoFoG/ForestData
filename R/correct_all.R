#' Full correction of a Forest Inventories Dataset
#'
#' Performs corrections for tree life status, overgrown recruits cases, size measurement errors and POM changes.
#'
#' @param data
#' @param id_col
#' @param time_col
#' @param status_col
#' @param POM_col
#' @param size_col
#' @param measure_type
#' @param plot_col
#' @param byplot
#' @param dead_confirmation_censuses
#' @param positive_growth_threshold
#' @param negative_growth_threshold
#' @param default_POM
#' @param dbh_min
#' @param use_size
#'
#' @return
#' @export
#'
#' @examples
correct_all <- function(data,
                        id_col = "idTree",
                        time_col = "CensusYear",
                        status_col = "CodeAlive",
                        POM_col,
                        size_col = "Circ",
                        measure_type = "circumference",
                        plot_col = "Plot",
                        byplot = TRUE,
                        dead_confirmation_censuses = 2,
                        positive_growth_threshold,
                        negative_growth_threshold,
                        default_POM,
                        dbh_min = 10,
                        use_size = FALSE){

  data <- correct_alive(data,
                        id_col,
                        time_col,
                        alive_col = status_col,
                        plot_col,
                        byplot,
                        dead_confirmation_censuses,
                        use_size)

  data <- correct_recruits(data,
                           dbh_min,
                           diameter_growth_limit = positive_growth_threshold,
                           time_col,
                           id_col,
                           plot_col,
                           size_col,
                           status_col,
                           measure_type,
                           byplot,
                           corrected_status)
  data <- correct_size(data,
                       size_col,
                       time_col,
                       status_col,
                       id_col,
                       POM_col,
                       positive_growth_threshold,
                       negative_growth_threshold,
                       default_POM)

  return(data)
}

