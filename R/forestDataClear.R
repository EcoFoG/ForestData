#' Clear Circumference
#'
#'
#'
#' @param data Dataframe containing data to clear
#' @param idTree Name of the id of the Tree column in the dataframe
#' @param Measure Name of the Measure column in the dataframe
#' @param MeasureDate Name of the Measure Date column in the dataframe
#' @param Status Name of the status of the tree column in the dataframe (if it doesn't exists in your dataset the function generate it from the NA in the dataframe)
#' @param idTaxon Name of the id of the taxon column in the dataframe (if it doesn't exists in your dataset you must specify the column of the genus and the specie)
#' @param Genus Name of the Genus column in the dataframe (if it doesn't exists in your dataset you must to specify the column name of idTaxon)
#' @param Specie Name of the Specie column in the dataframe (if it doesn't exists in your dataset you must to specify the column name of idTaxon)
#' @param replace When set to false it create a column circumf_corr, when set to true it replace measure named column with the corrected values
#'
#' @return a Dataframe cleared
#' @export
#'
#' @examples
#' clearCircumference(data, idTree = "idTree", Measure = "Circ", MeasureDate = "Year", Status = "Status", Genus = "Genus", Specie = "Specie", replace = FALSE)
#'
clearCircumference <- function(data, idTree = "idTree", Measure = "Circ", MeasureYear = "CensusYear", Status = FALSE, idTaxon = FALSE, Genus = "Genus", Specie = "Species", returnFullDataframe = FALSE)
{
  stopifnot(is.data.frame(data),is.character(idTree), is.character(Measure), is.character(MeasureDate))

  if (is.character(idTaxon)) {

  } else if(identical(idTaxon,FALSE) && (is.character(Genus) && is.character(Specie))){ # If idTaxon doesn't exists generate it with the couple Genus and Species
    data$idTaxon <- paste(data[[Genus]],data[[Specie]])
    data$idTaxon <- as.numeric(as.factor(data$idTaxon))
  } else {
    stop("idTaxon or the couple Genus and Specie must be defined")
  }

  if (is.character(Status)) {

  } else if(identical(Status,FALSE)) { # if Status column doesn't exists create it with the tree.status function of the corrections.R code
    data <- data %>%
      group_by_(idTree) %>%
      mutate_(status = tree.status(idTree))

    Status <- "status"
  }

  data[[Measure]] <- data[[Measure]]/(pi)
  if (returnFullDataframe) {
    data <- data %>% # Apply functions repl_missing and mega_correction from the corrections.R file to the dataframe
      group_by_(idTree) %>%
      mutate_(.dots=setNames(
        mega_correction(Measure, MeasureDate, Status),
        Measure
      ))

    data[[Measure]] <- data[[Measure]]*(pi)
  } else {
    data <- data %>% # Apply functions repl_missing and mega_correction from the corrections.R file to the dataframe
      group_by_(idTree) %>%
      mutate_(circumf_corr = mega_correction(Measure, MeasureDate, Status))

    data[["circumf_corr"]] <- data[["circumf_corr"]]*(pi)
  }


  data[[Measure]] <- data[[Measure]]*(pi)
  return(data)
}

clearDiameter <- function(data, idTree = "idTree", Measure = "Circ", MeasureYear = "CensusYear", Status = FALSE, idTaxon = FALSE, Genus = "Genus", Specie = "Species", returnFullDataframe = FALSE) # Same as clearCirconference but the function cut looks more logic for user
{
  stopifnot(is.data.frame(data),is.character(idTree), is.character(Measure), is.character(MeasureDate))

  if (is.character(idTaxon)) {

  } else if(identical(idTaxon,FALSE) && (is.character(Genus) && is.character(Specie))){ # If idTaxon doesn't exists generate it with the couple Genus and Species
    data$idTaxon <- paste(data[[Genus]],data[[Specie]])
    data$idTaxon <- as.numeric(as.factor(data$idTaxon))
  } else {
    stop("idTaxon or the couple Genus and Specie must be defined")
  }

  if (is.character(Status)) {

  } else { # if Status column doesn't exists create it with the tree.status function of the corrections.R code
    data <- data %>%
      group_by_(idTree) %>%
      mutate_(status = tree.status(idTree))

    Status <- "status"
  }

  if (returnFullDataFrame) {
    data <- data %>% # Apply functions repl_missing and mega_correction from the corrections.R file to the Measure column specified by user
      group_by_(idTree) %>%
      mutate_(.dots=setNames(
        mega_correction(Measure, MeasureDate, Status),
        Measure
      ))
  } else {
    data <- data %>% # Apply functions repl_missing and mega_correction from the corrections.R file to a new column
      group_by_(idTree) %>%
      mutate_(dbh_corr = mega_correction(Measure, MeasureDate, Status))
  }

  return(data)

}


