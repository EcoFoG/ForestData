#' Full correction of a Forest Inventories Dataset
#'
#' correct_all performs corrections for tree life status, overgrown recruits
#' cases, size measurement errors and POM changes, by calling correct_alive,
#' correct_size and correct_recruits in this order.
#'
#' @param data data.frame, containing forest inventories in the form of a
#'   long-format time series - one line corresponds to a measurement for one
#'   individual at a given census time.
#' @param id_col character, name of the column containing trees unique IDs.
#' @param time_col character, name of the column containing census years.
#' @param status_col character, name of the column corresponding to tree status:
#'   0/FALSE for dead, 1/TRUE for alive.
#' @param size_col character, name of the column corresponding to tree size
#'   (circumference or diameter) measurements .
#' @param measure_type character, partially matching “Circumference” or
#'   “Diameter”, indicating what is the type of the measurements.
#' @param dead_confirmation_censuses integer, defaults to 2: number of
#'   consecutive censuses for which a tree is unseen that are needed to consider
#'   the tree as dead. NB: for the trees unseen during the
#'   dead_confirmation_censuses -1 last inventories, the status cannot be
#'   corrected, thus mortality rates should not be calculated for these
#'   censuses.
#' @param plot_col  character, name of the column containing plot indices or
#'   names.
#' @param byplot logical, indicating whether the function has to process the
#'   data by plot (TRUE)or for the whole dataset (FALSE).
#' @param POM_col character, name of the column corresponding the Point Of
#'   Measurement (POM).
#' @param positive_growth_threshold positive numeric or integer, threshold over
#'   which an annual DIAMETER growth is considered abnormal (in cm). Defaults to
#'   5 cm.
#' @param negative_growth_threshold negative numeric or integer, threshold under
#'   which an absolute DIAMETER difference  is considered abnormal. To be given
#'   in centimeters. Defaults to -2 cm. Note that this threshold is applied
#'   between two consecutive censuses, regardless of the time between them, as
#'   it assumes that a tree diameter cannot decrease more than this value, even
#'   over a long period.
#' @param default_POM scalar numeric, default POM used in the dataset, in the
#'   same unit as the POM. When the value in POM_col is different from
#'   default_POM, the corrected size is given at default_POM  . It defaults to
#'   1.3 meters-according to current practice of measurement of diameter at
#'   breast height (DBH).
#' @param dbh_min scalar integer or numeric, indicating the minimum DIAMETER (in
#'   centimeters) at the default measurement height from which trees are
#'   recorded. Defaults to 10 cm.
#' @param use_size character, defaults to FALSE. Optional argument specifying
#'   whether to use measurement column (circumference or diameter) to create a
#'   vital status field  in case it does not already exist. See Details.
#' @param invariant_columns character vector, containing the name of the columns
#'   for which value remain constant for a given tree (for example species name
#'   or coordinates). When a row is added by the function correct_alive, values
#'   for invariant columns are taken from the value for other censuses. Defaults
#'   to null
#' @param species_col character, name of th column containing full species names
#'   (or other taxonomic identification)
#' @param pioneers character vector containing full species name (or other
#'   taxonomic identification)for which a specific positive growth threshold
#'   (used for instance for fast growing species for which the threshold to
#'   detect an abnormal growth is high).
#' @param pioneers_treshold Positive DIAMETER growth limit to apply to pioneer
#'   species (specified in 'pioneers'), similar to . Expressed in centimeters.
#'   Defaults to 7.5 cm
#'
#' @return A data.frame with additional columns: status_corr and size_corr for
#'   corrected tree vital status and size, code_corr for correction tag and
#'   types.
#'
#' @examples
#'
#' data(example_census)
#'
#' # Short version with parameters specified using prepare_forestdata and default values
#'
#' prepare_forestdata(example_census,plot_col="Plot",id_col="idTree",time_col="CensusYear", status_col = "CodeAlive",size_col="Circ",measure_type = "C",POM_col = "POM")
#'
#' correct_all(example_census,
#' invariant_columns = c("Genus",
#'                       "Species",
#'                       "binomial_name",
#'                       "Forest",
#'                       "Family"),
#' species_col = "binomial_name",#tag pioneer
#' measure_type = getOption("measure_type"),
#' pioneers = c("Cecropia","Pourouma"),#tag pioneer
#' pioneers_treshold = 7.5)
#'
#' # Full call:
#'
#' correct_all(example_census,
#' id_col = "idTree",
#' time_col = "CensusYear",
#' status_col = "CodeAlive",
#' plot_col = "Plot",
#' byplot = TRUE,
#' dead_confirmation_censuses = 2,
#' use_size = FALSE,
#' invariant_columns = c("Genus",
#'                       "Species",
#'                       "binomial_name",
#'                       "Forest",
#'                       "Family"),
#' size_col = "Circ",
#' species_col = "binomial_name",#tag pioneer
#' POM_col = "POM",
#' measure_type = "C",
#' positive_growth_threshold = 5,
#' negative_growth_threshold = -2,
#' default_POM = 1.3,
#' pioneers = c("Cecropia","Pourouma"),#tag pioneer
#' pioneers_treshold = 7.5,
#' dbh_min = 10)
correct_all <- function(data,
                        id_col = getOption("id_col"),
                        time_col = getOption("time_col"),
                        status_col = getOption("status_col"),
                        plot_col = getOption("plot_col"),
                        byplot = TRUE,
                        dead_confirmation_censuses = 2,
                        use_size = FALSE,
                        invariant_columns = c("Genus",
                                              "Species",
                                              "binomial_name",
                                              "Forest",
                                              "Family"),
                        size_col = getOption("size_col"),
                        species_col = "binomial_name",#tag pioneer
                        POM_col = getOption("POM_col"),
                        measure_type = getOption("measure_type"),
                        positive_growth_threshold = 5,
                        negative_growth_threshold = -2,
                        default_POM = 1.3,
                        pioneers = c("Cecropia","Pourouma"),#tag pioneer
                        pioneers_treshold = 7.5,
                        dbh_min = 10){

  data <- correct_alive(data,
                        id_col = id_col,
                        time_col = time_col,
                        status_col = status_col,
                        plot_col = plot_col,
                        byplot = byplot,
                        dead_confirmation_censuses = dead_confirmation_censuses,
                        use_size = use_size,
                        invariant_columns = invariant_columns)

  data <- correct_size(data,
                       dbh_min = 10,
                       positive_growth_threshold = 5,
                       time_col = time_col,
                       id_col = id_col,
                       plot_col = plot_col,
                       size_col = size_col,
                       status_col = "status_corr",
                       measure_type = measure_type,
                       byplot = byplot,
                       correct_status = FALSE)

  data <- correct_recruits(data,
                           size_col = size_col,
                           time_col = time_col,
                           status_col = "status_corr",
                           species_col = species_col,#tag pioneer
                           id_col = id_col,
                           POM_col = POM_col,
                           measure_type = measure_type,
                           positive_growth_threshold = positive_growth_threshold,
                           negative_growth_threshold = negative_growth_threshold,
                           default_POM = default_POM,
                           pioneers = pioneers,#tag pioneer
                           pioneers_treshold = pioneers_treshold)


  return(data)
}



