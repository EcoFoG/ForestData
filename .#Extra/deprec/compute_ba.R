#' Title
#'
#' @param data
#' @param measure
#' @param surface
#' @param forest_col
#' @param forest
#' @param plot_col
#' @param plot
#' @param subplot_col
#' @param subplot
#' @param time_col
#' @param time
#' @param taxon_col
#' @param taxon
#' @param Dmin
#' @param Dmax
#' @param categories
#'
#' @return
#' @export
#'
#' @examples
compute_ba <- function(data,
                       measure = "Diameter",
                       surface = NA,
                       forest_col = "Forest",
                       forest = NA,
                       plot_col = "Plot",
                       plot = NA,
                       subplot_col = "Subplot",
                       subplot = NA,
                       time_col = "CensusYear",
                       time = NA,
                       taxon_col = c("Genus","Species"),
                       taxon = NA,
                       Dmin = NA,
                       Dmax = NA,
                       categories = NA,
                       aggregation = T
){

  # General checks ----------------------------------------------------------

  if(!measure%in%c("Circumference",
                   "Diameter",
                   "circumference",
                   "diameter",
                   "Circ",
                   "Diam",
                   "circ" ,
                   "diam",
                   "C",
                   "D",
                   "c",
                   "d")){
    stop("Please indicate the type of measurement used : \"Circonference\" or \"Diameter\"")
  }
  if(!is.numeric(data) & !is.data.frame(data)){
    stop("The input dataset must be either numeric or a data.frame")
  }

  # If the dataset is just an array of measurements -------------------------

  if(is.numeric(data)){
    if(!is.na(surface)){
      print(paste0("The total basal area in the plot is ", ba, " squarred meters, correcponding to ", ba_perha, " squarred meters per ha."))
      return(ba, ba_perha)
    }
    else{
      print(paste0("The total basal area in the plot is ", ba, " squarred meters."))
      return(ba)
    }
  }


  # If the dataset is a data.frame ------------------------------------------
  else if(is.data.frame(data)){

    # Specific checks and beforehand treatments

    ## Forest

    if(!is.na(forest_col)){
      if(!forest_col%in%names(data))
        stop("The column name you indicated for forests (biological stations) is wrong. Please check it.")
      names(data[,which(names(data) == forest_col)]) <- "forest"
      if(!is.na(forest)){
        data <- data[which(data$forest == forest),]
        if(nrow(data) == 0)
          stop(paste("There is no forest (biological station) corresponding to", forest))
      }
    }
    ## Plots
    if(!is.na(plot_col)){
      if(!plot_col%in%names(data))
        stop("The column name you indicated for plots is wrong. Please check it.")
      names(data[,which(names(data) == plot_col)]) <- "plot"
      if(!is.na(plot)){
        data <- data[which(data$plot == plot),]
        if(nrow(data) == 0)
          stop(paste("There is no plot corresponding to", plot))
      }
    }
    ## Subplots
    if(!is.na(plot_col)){
      if(!subplot_col%in%names(data))
        stop("The column name you indicated for subplots is wrong. Please check it.")
      names(data[,which(names(data) == subplot_col)]) <- "subplot"
      if(!is.na(subplot)){
        data <- data[which(data$subplot == subplot),]
        if(nrow(data) == 0)
          stop(paste("There is no subplot corresponding to", subplot))
      }
    }

    ## Census campaigns
    if(!is.na(time_col)){
      if(!time_col%in%names(data))
        stop("The column name you indicated for times is wrong. Please check it.")
      names(data[,which(names(data) == time_col)]) <- "time"
      if(!is.na(time)){
        data <- data[which(data$time == time),]
        if(nrow(data) == 0)
          stop(paste("There is no time corresponding to", time))
      }
    }

    ## Taxa
    if(!is.na(taxon_col)){
      if(length(taxon_col)> 1){ # In some inventories, genus and species name are separated so that the binomial name is a transformed field that users have to create.
        # In this case, the user must have the choice between doing it beforehand or indicating the name of the fields to compose species (or even, subspecies, whatever) name.
        data$taxon = taxon_col[1]
        for(level in 2:length(taxon_col)){
          data$taxon <- paste(data$taxon, data[,which(names(data) == taxon_col[level])], sep = "_")
        }
      }
      else{
        if(!taxon_col %in% names(data)) stop("The name you indicated for taxon column is apparently not in the dataset's names. Please indicate the right one, or unactivate this feature with taxon_col = NA")
        names(data[which(names(data) == taxon_col)]) <- "taxon"
      }
    }
    if(!is.na(taxon)){
      data <- data[which(data$taxon) == taxon,]
      if(nrow(data) == 0) stop("There is no measure available for the taxon you indicated. The taxon name must follow the format of this example : genusnames_speciesname ; althought it is possible to add levels - always separated by an underscore_")
    }
    if(measure%in%c("Circumference",
                    "circumference",
                    "Circ",
                    "circ",
                    "C",
                    "c")){
      data <- data/pi
      print("Circumferences have been converted to diameter. If your measures are already diameters, please indicate it with the measure argument")
    }

    ## Min and max diameters
    if(!is.na(Dmin)){
      data <- data[which(data$measure >= Dmin),]
      if(nrow(data == 0))
        stop("there is no tree above the minimum diameter you indicated")
    }
    if(!is.na(Dmax)){
      data <- data[which(data$measure >= Dmax),]
      if(nrow(data == 0))
        stop("there is no tree under the maximum diameter you indicated")
    }

    # Individual level BA calculus

    # Aggregate to sum according to user specified filters
    if(aggregation){
      arglist <- ifelse(!is.na(taxon_col),data.frame(forest_col, plot_col, subplot_col, time_col, "taxon"),data.frame(forest_col, plot_col, subplot_col, time_col))
      arglist <- arglist[which(!is.na(arglist))]
      for(i in 1:ncol(arglist)){

      }
    }
    else return(data)
  }
}
