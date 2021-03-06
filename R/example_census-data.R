#' Example Forest Census to Illustrate ForestData Functions
#'
#' This dataset was obtain from the Paracou database and exclusively aims at
#' illustrating ForestData features. It has been purposedly modified: plot 6
#' square 1 is provided as-is, but plot 6 square 3 misses several years to
#' simulate two plots with different censusing resolution.
#'
#' @docType data
#'
#' @usage data(example_census)
#'
#' @format An object of class \code{"data.frame"} with \itemize{
#'   \item{Forest}{The location of the plots included in the dataset}
#'   \item{Plot}{The plot indices correspondinc to the plot a given tree belongs
#'   to - In this dataset, the plots all correspond to subplots of Paracou's
#'   Plot6 (control, undisturbed)} \item{idTree}{Unique tree ID}
#'   \item{Family}{Taxonomical information, the plant family of each tree}
#'   \item{Genus}{Taxonomical information, the genus of each tree}
#'   \item{Species}{Taxonomical information, the species name of each
#'   tree} \item{binomial_name}{Taxonomical information, the complete binomial
#'   name (genus_species) of each tree} \item{CensusYear}{The census year at which each data have been acquired} \item{CodeAlive}{Whether the tree is dead (0) or alive (1). NAs do not exist in the original example dataset but can be found after using correct_alive (see the documentation)}
#'   \item{Circ}{The circumference of the tree} \item{POM}{The Point of
#'   Measurement (in meters) for each data}}
#'
#' @keywords datasets
#'
#' @references The Paracou Research Station and Forest sDisturbance Experiment
#'   (\href{https://paracou.cirad.fr/}{Paracou Research Station})
#'
#' @source \href{https://paracou.cirad.fr/}{On demand}
#'
#' @examples
#' data(example_census)
"example_census"
