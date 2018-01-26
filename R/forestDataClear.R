source("R/corrections.R")
library("dplyr")
forestData.clearCircumference <- function(data, idTree = "idTree", Measure = "Circ", MeasureDate = "Year", Status = FALSE, idTaxon = FALSE, Genus = FALSE, Specie = FALSE, replace = FALSE)
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

  data[[MeasureDate]] <- as.Date(data[[MeasureDate]]) # Convert in date
  data[[MeasureDate]] <- as.numeric(format(data[[MeasureDate]], "%Y")) # Extract Year from the date
  data[[Measure]] <- data[[Measure]]/(pi)
  if (replace) {
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

forestData.clearDiameter <- function(data, idTree, Measure, MeasureDate, Status = FALSE, idTaxon = FALSE, Genus = FALSE, Specie = FALSE, returnFullDataFrame = TRUE) # Same as clearCirconference but the function cut looks more logic for user
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

  data[[MeasureDate]] <- as.Date(data[[MeasureDate]]) # Convert in date
  data[[MeasureDate]] <- as.numeric(format(data[[MeasureDate]], "%Y")) # Extract Year from the date

  if (replace) {
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
forestData.clearCirconference(data, "ffqsdf", "dsfsdfsdf", "rhfgdssfdghdgf","gdfgsdfgsdf","dfgsdfg")

