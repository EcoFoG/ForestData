#' Example dataset for ForestData: Paracou, plot 6
#'
#' A built-in example dataset used in ForestData's examples and vignette. It
#' corresponds to the squares 1 and 3 of the 6th Plot of the Paracou Experiment
#' database
#'
#' @format A data frame with 25533 rows and 12 variables:
#' \describe{
#'   \item{Forest}{Character, general information: Forest sector where the plot
#'   to which trees belong is located. Here, always Paracou:P6}
#'   \item{Plot}{Integer (could be character), the index of the plot to which
#'   trees belong}
#'   \item{idTree}{Integer (could be character), the unique id of
#'   the tree}
#'   \item{Family}{Character, taxonomic information : botanical
#'   family}
#'   \item{Genus}{Character, taxonomic information : botanical genus}
#'   \item{Species}{Character, taxonomic information : species name}
#'   \item{CensusYear}{Integer, the year at which the tree is censused}
#'   \item{CodeAlive}{Logical or integer, tree life status: 1 for alive, 0 for
#'   dead. NA, if any, indicate that the tree was not seen for the corresponding
#'   census}
#'   \item{CodeMeas}{Character, not important here. Additionan
#'   informations relative to measurements}
#'   \item{Circ}{Numeric. Tree size
#'   expressed as stem circumference, in centimeters}
#'   \item{binomial_name}{Taxonomic information: full binomial species name}
#'   \item{POM}{Numeric. Point of Measurement, in meters.}
#'   }
#' @source \url{https://paracou.cirad.fr/}
"example_census"

#' Example dataset for ForestData: Paracou, plot 6, with corrected status
#'
#' The same built-in example dataset as \code{example_census}, corrected with
#' \code{correct_alive}
#'
#' @format A data frame with 25634 rows and 13 variables:
#' \describe{
#'   \item{Forest}{Character, Forest sector where the plot is located}
#'   \item{Plot}{Integer (could be character), the index of the plot to which
#'   trees belong}
#'   \item{idTree}{Integer (could be character), the unique id of
#'   the tree}
#'   \item{Family}{Character, taxonomic information : botanical
#'   family}
#'   \item{Genus}{Character, taxonomic information : botanical genus}
#'   \item{Species}{Character, taxonomic information : species name}
#'   \item{CensusYear}{Integer, the year at which the tree is censused}
#'   \item{CodeAlive}{Logical or integer, tree life status: 1 for alive, 0 for
#'   dead. NA, if any, indicate that the tree was not seen for the corresponding
#'   census}
#'   \item{CodeMeas}{Character, not important here. Additionan
#'   informations relative to measurements}
#'   \item{Circ}{Numeric. Tree size
#'   expressed as stem circumference, in centimeters}
#'   \item{binomial_name}{Taxonomic information: full binomial species name}
#'   \item{POM}{Numeric. Point of Measurement, in meters.}
#'   \item{status_corr}{Integer, interpretable as logical. Corrected vital
#'   status obtained with \code{correct_alive}}
#'   }
#' @source \url{https://paracou.cirad.fr/}
"example_status_corr"

#' Example dataset for ForestData: Paracou, plot 6, corrected for status and
#' size
#'
#' The same built-in example dataset as \code{example_census}, corrected with
#' \code{correct_alive} then with \code{correct_size}
#'
#' @format A data frame with 25634 rows and 12 variables:
#' \describe{
#'   \item{Forest}{Character, general information: Forest sector where the plot
#'   to which trees belong is located. Here, always Paracou:P6}
#'   \item{Plot}{Integer (could be character), the index of the plot to which
#'   trees belong}
#'   \item{idTree}{Integer (could be character), the unique id of
#'   the tree}
#'   \item{Family}{Character, taxonomic information : botanical
#'   family}
#'   \item{Genus}{Character, taxonomic information : botanical genus}
#'   \item{Species}{Character, taxonomic information : species name}
#'   \item{CensusYear}{Integer, the year at which the tree is censused}
#'   \item{CodeAlive}{Logical or integer, tree life status: 1 for alive, 0 for
#'   dead. NA, if any, indicate that the tree was not seen for the corresponding
#'   census}
#'   \item{CodeMeas}{Character, not important here. Additionan
#'   informations relative to measurements}
#'   \item{Circ}{Numeric. Tree size
#'   expressed as stem circumference, in centimeters}
#'   \item{binomial_name}{Taxonomic information: full binomial species name}
#'   \item{POM}{Numeric. Point of Measurement, in meters.}
#'   \item{status_corr}{Integer, interpretable as logical. Corrected vital
#'   status obtained with \code{correct_alive}}
#'   \item{size_corr}{Numeric.
#'   Corrected tree size (here, circumference) obtained with \code{correct_size}}
#'   \item{code_corr}{Character, indicates the type of correction done for each
#'   measurement, if any. Obtained with \code{correct_size}}
#'   }
#' @source \url{https://paracou.cirad.fr/}
"example_size_corr"

#' Example dataset for ForestData: Paracou, plot 6, corrected for status and
#' size, then for overgrown recruits
#'
#' The same built-in example dataset as \code{example_census}, corrected with
#' \code{correct_alive}, \code{correct_size} and \code{correct_recruits}
#'
#' @format A data frame with 25634 rows and 12 variables:
#' \describe{
#'   \item{Forest}{Character, general information: Forest sector where the plot
#'   to which trees belong is located. Here, always Paracou:P6}
#'   \item{Plot}{Integer (could be character), the index of the plot to which
#'   trees belong} \item{idTree}{Integer (could be character), the unique id of
#'   the tree} \item{Family}{Character, taxonomic information : botanical
#'   family} \item{Genus}{Character, taxonomic information : botanical genus}
#'   \item{Species}{Character, taxonomic information : species name}
#'   \item{CensusYear}{Integer, the year at which the tree is censused}
#'   \item{CodeAlive}{Logical or integer, tree life status: 1 for alive, 0 for
#'   dead. NA, if any, indicate that the tree was not seen for the corresponding
#'   census} \item{CodeMeas}{Character, not important here. Additionan
#'   informations relative to measurements} \item{Circ}{Numeric. Tree size
#'   expressed as stem circumference, in centimeters}
#'   \item{binomial_name}{Taxonomic information: full binomial species name}
#'   \item{POM}{Numeric. Point of Measurement, in meters.}
#'   \item{status_corr}{Integer, interpretable as logical. Corrected vital
#'   status obtained with \code{correct_alive}} \item{size_corr}{Numeric.
#'   Corrected tree size (here, circumference) obtained with \code{correct_size}}
#'   \item{code_corr}{Character, indicates the type of correction done for each
#'   measurement, if any. Obtained with \code{correct_size}}
#'   \item{corrected_recruit}{Logical. Flags the lines added by \code{correct_recruits} if any}
#'   }
#' @source \url{https://paracou.cirad.fr/}
"example_recruits"
