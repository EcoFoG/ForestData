#' Full correction of a Forest Inventories Dataset
#'
#' Performs corrections for tree life status, overgrown recruits cases, size measurement errors and POM changes.
#'
#' @param data Data.frame, no default. Forest inventory in the form of a long-format time series - one line is a measure for one individual during one census time.
#' @param id_col Character. The name of the column containing trees unique ids
#' @param time_col Character. The name of the column containing census year
#' @param status_col Character. The name of the column containing tree vital status - 0=dead; 1=alive.
#' @param size_col Character. The name of the column containing tree size measurements - diameter or circumference at breast height
#' @param measure_type A single character indicating whether tree sizes are given in circumferences -"C"- or diameter -"D"-.
#' @param dead_confirmation_censuses Integer, defaults to 2. This is the number of censuses needed to state that a tree is considered dead, if unseen. In Paracou, we use the rule-of-thumb that if a tree is unseen twice, its probability to be actually dead is close to 1. The choice of this value involves that trees unseen during the X-1 last inventories can not be corrected for death, and thus mortality rates should not be calculated for these censuses.
#' @param plot_col Character. The name of the column containing the plots indices.
#' @param byplot Logical. If there are several plots in your dataset, the correction is performed by plot, in case these would not be censuses the same years or with the same frequencies one another.
#' @param POM_col Character. The name of the column containing trees POM
#' @param positive_growth_threshold Numeric. Upper threshold over which an annual DIAMETER growth is considered abnormal. Defaults to 5 cm.
#' @param negative_growth_threshold Numeric. Lower threshold under which a negative annual DIAMETER growth is considered abnormal. Defaults to -2 cm.
#' @param default_POM Numeric. POM normally used in the forest inventory- defaults to the internationa convention of breast height, 1.3
#' @param dbh_min A scalar, integer or numeric, indicating the minimum DIAMETER at breast height at which trees are recorded, in centimeters.
#' @param use_size Character, but defaults to FALSE. Optional argument specifying that circumference or diameter must be used to create the vital status field. If your data already contains a field indicating whether the tree is dead -0 or FALSE- or alive -1 or TRUE-, let it to its default value. If you use this option, make sure beforehand that only live trees are measured - non-NA size - in your dataset's protocol.
#'
#' @return A data.frame with additional columns: status_corr and size_corr for corrected tree vital status and size, code_corr for correction tag and types.
#' @export
#'
#' @examples
#' \dontrun{
#' correct_all <- function(data,
#' id_col = "idTree",
#' time_col = "CensusYear",
#' status_col = "CodeAlive",
#' POM_col,
#' size_col = "Circ",
#' measure_type = "circumference",
#' plot_col = "Plot",
#' byplot = TRUE,
#' dead_confirmation_censuses = 2,
#' positive_growth_threshold,
#' negative_growth_threshold,
#' default_POM,
#' dbh_min = 10,
#' use_size = FALSE)}
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
                        status_col,
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

